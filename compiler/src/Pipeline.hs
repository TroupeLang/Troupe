-- | The compile pipeline from source text to optimized IR.
--
-- This is the front end as a library function, so that anything needing IR the
-- real pipeline produced — a test suite, a differential harness, a service —
-- can obtain it without going through the @troupec@ executable. Before this
-- module existed the pipeline was assembled inside the executable, and the only
-- way to reach it was to add a flag; that is how @--verify-ir-sexp@ came to be
-- a compiler option instead of a test.
--
-- Two stages, because the datatype-hash report is taken between them:
--
--   * 'frontEndFold' — parse, resolve imports, re-associate operators, extract
--     the export list, fold syntactic variants.
--   * 'frontEndIR' — pattern-match elimination through closure conversion and
--     IR optimization.
--
-- Errors still terminate the process (via 'die' here and in "ProcessImports"),
-- which is the behaviour the executable has always had. A caller that must
-- survive a bad input — the corpus test does — catches the 'ExitCode'
-- exception. Threading errors out as values instead is a separate change,
-- touching every 'die' site in "ProcessImports" and constrained by the golden
-- tests over compiler error messages.
module Pipeline
  ( CompileOpts(..)
  , StageDump(..)
  , silentDump
  , Folded(..)
  , frontEndFold
  , frontEndIR
  , writeUnitExports
  , compileModuleGraph
  , moduleOutPath
  ) where

import           Control.Monad.Except (runExcept)
import           System.Exit (die)

import qualified CaseElimination as C
import qualified ClosureConv as CC
import           CompileMode (CompileMode(..))
import qualified Core
import qualified Direct
import qualified Surface
import           DepsFile (DepEntry)
import           Exports (extractExports, exportsFileContent)
import           Basics (isOperatorName)
import qualified Data.Map as Map
import qualified ModuleHash
import           OpReassoc (fileFixityEnv)
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath (takeDirectory, takeBaseName, takeExtension, dropExtension, (</>))
import           Util.FileUtil (atomicWriteFileD)
import qualified IR
import qualified IROpt
import           OpReassoc (reassocProg)
import           ProcessImports (discoverModules)
import           Parser (parseProg)
import           PrettyPrint (PPConfig, runPP)
import           ProcessImports (PinCheck, processImports)
import qualified RetDFCPS
import qualified CPSOpt
import           ShowIndent (showIndent)
import qualified SynVarFolding as SVF
import qualified Text.PrettyPrint.HughesPJ as PP

-- | Where a stage's intermediate output goes. The executable writes files under
-- @out/@ and prints separators; everything else passes 'silentDump'.
data StageDump = StageDump
  { dumpSep  :: String -> IO ()             -- ^ section header
  , dumpFile :: FilePath -> String -> IO () -- ^ per-stage dump file
  , dumpEcho :: String -> IO ()             -- ^ echo to stdout (the syntax dump)
  }

silentDump :: StageDump
silentDump = StageDump (\_ -> return ()) (\_ _ -> return ()) (\_ -> return ())

data CompileOpts = CompileOpts
  { coMode     :: CompileMode
  , coDump     :: StageDump
  , coPPConfig :: PPConfig
  }

-- | What the folding stage produced, and what the rest of a compile needs from
-- it beyond the IR itself.
data Folded = Folded
  { fdSurface  :: Surface.Prog          -- ^ post-import program; source of the fixity environment
  , fdProg     :: Direct.Prog           -- ^ after operator re-association; carries the import list
  , fdFolded   :: Direct.Prog           -- ^ variants folded
  , fdLocal    :: [(String, String)]    -- ^ (group hash, canonical form) declared here
  , fdConsumed :: [(String, [String])]  -- ^ (library, consumed group hashes)
  , fdExports  :: Maybe [String]        -- ^ export list, for a library compile
  , fdDeps     :: [DepEntry]            -- ^ module dependencies this file resolved
  }

-- | Parse through syntactic-variant folding. @root@ is the project root that
-- module imports resolve and display against; @file@ is this compilation unit.
frontEndFold :: PinCheck -> FilePath -> CompileOpts -> FilePath -> String -> IO Folded
frontEndFold pin root opts file input = do
  let dump = coDump opts
  prog_parsed <- case parseProg file input of
                   Left err -> die err
                   Right p  -> return p

  (sprog, resolvedDeps) <- processImports pin root file prog_parsed

  -- The parse-phase program (flat operator chains); this is what the SYNTAX
  -- dump shows.
  dumpSep dump "SYNTAX"
  dumpFile dump "out/out.syntax" (showIndent 2 sprog)
  dumpEcho dump (showIndent 2 sprog)

  -- Re-association: rebuild operator chains into the Direct operator tree from
  -- the fixity environment.
  prog <- case runExcept (reassocProg sprog) of
            Right p -> return p
            Left s  -> die s
  dumpSep dump "OPERATOR REASSOCIATION"
  dumpFile dump "out/out.opreassoc" (showIndent 2 prog)

  exports <- case coMode opts of
               Library -> case runExcept (extractExports prog) of
                            Right es -> return (Just es)
                            Left s   -> die s
               _       -> return Nothing

  -- Syntactic-variant folding. A datatype constructor may shadow one of the
  -- ambient builtin names; the folder works on the program's own declarations.
  foldRes <- case runExcept (SVF.foldProg prog) of
               Right r -> return r
               Left s  -> die s
  let folded = SVF.frProg foldRes

  return Folded { fdSurface  = sprog
                , fdProg     = prog
                , fdFolded   = folded
                , fdLocal    = SVF.frLocal foldRes
                , fdConsumed = SVF.frConsumed foldRes
                , fdExports  = exports
                , fdDeps     = resolvedDeps
                }

-- | Pattern-match elimination through closure conversion and IR optimization.
frontEndIR :: CompileOpts -> Folded -> IO IR.IRProgram
frontEndIR opts fd = do
  let dump    = coDump opts
      ppConf  = coPPConfig opts
      ppDump f d = dumpFile dump f (PP.render (runPP ppConf d))

  prog' <- case runExcept (C.trans (coMode opts) (fdFolded fd)) of
             Right p -> return p
             Left s  -> die s
  dumpSep dump "PATTERN MATCH ELIMINATION"
  dumpFile dump "out/out.nopats" (showIndent 2 prog')

  let lowered = Core.lowerProg prog'
  dumpSep dump "LOWERING FUNS AND LETS"
  dumpFile dump "out/out.lowered" (showIndent 2 lowered)

  renamed <- case runExcept (Core.renameProg lowered) of
               Right p -> return p
               Left s  -> die ("troupec: " ++ s)
  dumpSep dump "α RENAMING"
  dumpFile dump "out/out.alpha" (showIndent 2 renamed)

  let cpsed = RetDFCPS.transProg renamed
  dumpSep dump "CPSED"
  dumpFile dump "out/out.cps" (showIndent 2 cpsed)

  let rwcps = CPSOpt.rewrite cpsed
  dumpSep dump "REWRITING CPS"
  dumpFile dump "out/out.cpsopt" (showIndent 2 rwcps)

  ir <- case runExcept (CC.closureConvert (coMode opts) rwcps) of
          Right ir -> return ir
          Left s   -> die ("troupec: " ++ s)
  ppDump "out/out.ir" (IR.ppProg ir)

  let iropt = IROpt.iropt ir
  ppDump "out/out.iropt" (IR.ppProg iropt)
  return iropt

------------------------------------------------------------
-- Interfaces and module graphs
------------------------------------------------------------

-- | Write a compilation unit's @.exports@ interface next to its output path.
-- A program-module artifact records its own content hash (the identity its
-- consumers pin); a stdlib library carries no such line.
writeUnitExports :: FilePath -> Bool -> Folded -> IR.IRProgram -> IO ()
writeUnitExports outPath isModuleArtifact fd iropt =
  case fdExports fd of
    Nothing -> return ()
    Just es -> do
      -- Every exported operator carries its fixity into the interface; a
      -- symbolic export without a fixity in the file's environment (local
      -- declarations plus unqualified imports, so re-export propagates) is an
      -- error.
      fixEnv <- case runExcept (fileFixityEnv (fdSurface fd)) of
                  Right m -> return m
                  Left s  -> die s
      exportedFixities <-
        mapM (\v -> case Map.lookup v fixEnv of
                      Just f  -> return (v, f)
                      Nothing -> die $ "exported operator '" ++ v
                                 ++ "' needs a fixity declaration in this file's header")
             (filter isOperatorName es)
      let moduleHashVal = if isModuleArtifact then Just (ModuleHash.moduleHash iropt) else Nothing
          path' = if takeExtension outPath == ".js" then dropExtension outPath else outPath
      atomicWriteFileD (path' ++ ".exports")
        (exportsFileContent moduleHashVal es (fdLocal fd) exportedFixities)

-- | Compile every module in a program's import graph, dependencies first,
-- writing each one's interface so the next unit can resolve against it.
-- @onUnit@ sees each module's front end and IR: the executable emits its
-- JavaScript there, the corpus test checks the s-expression laws.
--
-- The main unit is not compiled here — its caller has its own work to do with
-- it (flags, output path, early-exit modes).
compileModuleGraph :: PinCheck -> FilePath -> CompileOpts -> FilePath
                   -> (FilePath -> Folded -> IR.IRProgram -> IO ())
                   -> IO ()
compileModuleGraph pin root opts file onUnit = do
  mods <- discoverModules file
  mapM_ compileOne mods
  where
    compileOne m = do
      createDirectoryIfMissing True (takeDirectory m </> "out")
      minput <- readFile m
      folded <- frontEndFold pin root opts m minput
      iropt  <- frontEndIR opts folded
      writeUnitExports (moduleOutPath m) True folded iropt
      onUnit m folded iropt

-- | Where a module's artifacts go: @<dir>/out/<name>.js@ beside its source.
moduleOutPath :: FilePath -> FilePath
moduleOutPath f = takeDirectory f </> "out" </> takeBaseName f ++ ".js"
