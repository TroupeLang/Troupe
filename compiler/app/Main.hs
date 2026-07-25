{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified SynVarFolding as SVF
import Parser
import qualified Core as Core
import RetDFCPS
import qualified CaseElimination as C
import System.Environment
import Util.FileUtil
import qualified ClosureConv as CC
import qualified IR as CCIR
import qualified IROpt
-- import qualified RetRewrite as Rewrite
import qualified CPSOpt as CPSOpt
import qualified IR2Raw
import qualified IRSexp
import qualified Raw
import qualified Raw2Stack
import qualified Stack
import qualified Stack2JS
import qualified RawOpt
import qualified PrettyPrint as PPrint
import qualified Text.PrettyPrint.HughesPJ as PP
-- import System.IO (isEOF)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy.Char8 as BSLazyChar8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as Aeson
import System.IO
import TroupeSourceMap (buildSourceMap)
import System.Exit
import ProcessImports
import OpReassoc (reassocProg, fileFixityEnv)
import qualified OpReassoc
import Basics (isOperatorName)
import qualified Data.Map as Map
import qualified ModuleHash
import DepsFile (DepEntry(..), depsFilePath, readDepsFile, writeDepsFile, lookupPinByPath)
import Direct (Prog(..))
import Basics (Imports(..), importPath)
import System.Directory (createDirectoryIfMissing)
import AddAmbientMethods
import ShowIndent
import Exports
import CompileMode
import Control.Monad.Except
import Control.Monad (when)
import System.Console.GetOpt
import Data.List as List
import Data.Function (on)
import Data.Maybe (fromJust)
import System.FilePath
import qualified Data.Text as T

--------------------------------------------------------------------------------
----- COMPILER FLAGS -----------------------------------------------------------

data Flag
  = TextIRMode
  | JSONIRMode
  | LibMode
  | NoRawOpt
  | OutputFile String
  | Verbose
  | Help
  | Debug
  | SourceMap
  | DebugPP
  | PPPosFormat String
  | EmitIRSexp
  | IngestIRSexp
  | VerifyIRSexp
  -- | Establish/update the dependencies file for a program (the maintenance
  -- utility): resolve and hash the module graph and record actual hashes as
  -- pins, instead of enforcing existing pins. A driver-facing subcommand.
  | UpdateDeps
  -- | Internal (not a CLI option): mark a compile as a program-module artifact,
  -- so its @.exports@ carries the module's own content hash. Injected by the
  -- driver for module compiles; absent for stdlib library compiles.
  | ModuleArtifact
  | DatatypeHashes
  deriving (Show, Eq)

options :: [OptDescr Flag]
options =
  [ Option ['i'] ["text-ir"]   (NoArg TextIRMode)         "ir interactive mode (text)"
  , Option ['j'] ["json-ir"]   (NoArg JSONIRMode)         "ir interactive mode (json)"
  , Option []    ["no-rawopt"] (NoArg NoRawOpt)           "disable Raw optimization"
  , Option ['v'] ["verbose"]   (NoArg Verbose)            "verbose output"
  , Option ['d'] ["debug"]     (NoArg Debug)              "debugging information in the .js file"
  , Option ['l'] ["lib"]       (NoArg LibMode)            "compiling a library"
  , Option ['h'] ["help"]      (NoArg Help)               "print usage"
  , Option ['o'] ["output"]    (ReqArg OutputFile "FILE") "output FILE"
  , Option ['m'] ["source-map"] (NoArg SourceMap)         "generate source map"
  , Option []    ["debug-pp"]  (NoArg DebugPP)            "show positions in IR dumps"
  , Option []    ["pp-pos-format"] (ReqArg PPPosFormat "FMT") "position format: inline|comment|bracket|none"
  , Option []    ["emit-ir-sexp"]   (NoArg EmitIRSexp)   "compile a .trp and emit the IR as troupe-ir-sexp text (to -o FILE, else stdout)"
  , Option []    ["ingest-ir-sexp"] (NoArg IngestIRSexp) "read a troupe-ir-sexp file and compile it to JS (program must be self-contained; no ambient methods are injected)"
  , Option []    ["verify-ir-sexp"] (NoArg VerifyIRSexp) "compile a .trp, print its IR as troupe-ir-sexp, re-parse, and check the position-erased ASTs match (R1 self-check)"
  , Option []    ["update-deps"]    (NoArg UpdateDeps)   "establish/update the program's dependencies file (<main>.deps.json) with actual module hashes, instead of enforcing pins"
  , Option []    ["datatype-hashes"] (NoArg DatatypeHashes) "print the content hash and canonical form of each datatype group declared in the file, then stop"
  ]

--------------------------------------------------------------------------------
----- PIPELINE FROM FLAGS TO IR AND JS -----------------------------------------

-- | Compile one file. `root` is the project root (the main file's directory)
-- against which module imports are resolved and displayed. `pin` selects
-- whether the frontend enforces the dependencies file or establishes it.
-- Returns the module dependency entries this file resolved (for the maintenance
-- utility to record; ignored on a normal enforcing compile).
process :: PinCheck -> FilePath -> [Flag] -> Maybe String -> String -> IO ([DepEntry], ExitCode)
process pin root flags fname input = do
  let ast    = parseProg (maybe "" id fname) input

  let compileMode = if LibMode `elem` flags then Library else Normal

  let verbose = Verbose `elem` flags
      noRawOpt = NoRawOpt `elem` flags
      debugJS = Debug `elem` flags
      sourceMapEnabled = SourceMap `elem` flags
      debugPP = DebugPP `elem` flags
      isPPPosFormatFlag (PPPosFormat _) = True
      isPPPosFormatFlag _ = False
      ppPosFormatStr = case List.find isPPPosFormatFlag flags of
                         Just (PPPosFormat s) -> s
                         _ -> "inline"
      ppConfig = PPrint.mkPPConfig debugPP (PPrint.parsePosFormat ppPosFormatStr)

  case ast of
    Left err -> do
      die err

    Right prog_parsed -> do
      let outPath = outFile flags (fromJust fname)

      -- To print all tokens from the parser, uncomment the following line:
      -- debugTokens (Right tks) = mapM_ print tks

      ------------------------------------------------------
      -- TROUPE (FRONTEND) ---------------------------------
      -- Ambient-method injection is deferred to after syntactic-variant folding
      -- (see the fold block below); processImports therefore runs on the raw
      -- parsed program. Module imports resolve relative to this file, displayed
      -- relative to the project root.
      (sprog, resolvedDeps) <- processImports pin root (maybe "" id fname) prog_parsed

      -- The parse-phase program (flat operator chains); this is what the
      -- SYNTAX dump shows.
      when verbose $ do printSep "SYNTAX"
                        writeFileD "out/out.syntax" (showIndent 2 sprog)
                        putStrLn (showIndent 2 sprog)

      -- Re-association: rebuild operator chains into the Direct operator
      -- tree from the fixity environment (built-ins seeded; user operators
      -- from declarations and imports once those land).
      prog <- case runExcept (reassocProg sprog) of
                Right p -> return p
                Left s  -> die s

      exports <- case compileMode of Library -> case runExcept (extractExports prog) of
                                                     Right es -> return (Just (es))
                                                     Left s   -> die s
                                     _       -> return Nothing
      ------------------------------------------------------
      -- Syntactic-variant folding runs on the user program before the ambient
      -- methods are injected, so the folder never inspects the generated
      -- declarations and a datatype constructor may shadow an ambient builtin.
      foldRes <- case runExcept (SVF.foldProg prog) of
        Right r -> return r
        Left s -> die s
      let localGroups    = SVF.frLocal foldRes      -- [(group hash, canonical form)]
          consumedRecord = SVF.frConsumed foldRes   -- [(library, consumed hashes)]
          folded = case compileMode of Normal -> addAmbientMethods (SVF.frProg foldRes)
                                       _      -> SVF.frProg foldRes

      when (DatatypeHashes `elem` flags) $ do
        putStr (datatypeHashReport localGroups)
        exitSuccess

      prog' <- case runExcept (C.trans compileMode folded) of
        Right p -> return p
        Left s -> die s
      when verbose $ do printSep "PATTERN MATCH ELIMINATION"
                        writeFileD "out/out.nopats" (showIndent 2 prog')
      ------------------------------------------------------
      let lowered = Core.lowerProg prog'
      when verbose $ do printSep  "LOWERING FUNS AND LETS"
                        writeFileD "out/out.lowered" (showIndent 2 lowered)
      ------------------------------------------------------
      renamed <- case runExcept (Core.renameProg lowered) of
        Right p -> return p
        Left s -> die $ "troupec: " ++ s
      when verbose $ do printSep "α RENAMING"
                        writeFileD "out/out.alpha" (showIndent 2 renamed)
      ------------------------------------------------------
      let cpsed = RetDFCPS.transProg renamed
      when verbose $ do printSep "CPSED"
                        writeFileD "out/out.cps" (showIndent 2 cpsed)
      ------------------------------------------------------
      let rwcps = CPSOpt.rewrite cpsed
      when verbose $ do printSep  "REWRITING CPS"
                        writeFileD "out/out.cpsopt" (showIndent 2 rwcps)

      ------------------------------------------------------
      ------ IR (BACKEND) ----------------------------------
      ir <- case runExcept (CC.closureConvert compileMode rwcps) of 
          Right ir -> return ir 
          Left  s -> die $ "troupec: " ++ s

      when verbose $ writeFileD "out/out.ir" (PP.render $ PPrint.runPP ppConfig $ CCIR.ppProg ir)

      let iropt = IROpt.iropt ir
      when verbose $ writeFileD "out/out.iropt" (PP.render $ PPrint.runPP ppConfig $ CCIR.ppProg iropt)

      ------ EMIT troupe-ir-sexp (and stop) ----------------
      when (EmitIRSexp `elem` flags) $ do
        let sexp = IRSexp.printProg iropt
        case List.find isOutputFile flags of
          Just (OutputFile f) -> writeFile f sexp
          _                   -> putStr sexp
        exitSuccess

      ------ VERIFY troupe-ir-sexp round-trip (and stop) ---
      when (VerifyIRSexp `elem` flags) $ do
        let printed = IRSexp.printProg iropt
        case IRSexp.parseProg printed of
          Left err -> die ("troupe-ir-sexp round-trip FAILED (parse): " ++ err)
          Right ir2 ->
            if IRSexp.erasePosProg iropt == IRSexp.erasePosProg ir2
              then do putStrLn "troupe-ir-sexp round-trip OK"
                      exitSuccess
              else die "troupe-ir-sexp round-trip FAILED (position-erased ASTs differ)"

      ------ RAW -------------------------------------------
      let raw = IR2Raw.prog2raw iropt
      when verbose $ printSep  "GENERATING RAW"
      when verbose $ writeFileD "out/out.rawout" (PP.render $ PPrint.runPP ppConfig $ Raw.ppProg raw)

      ----- RAW OPT ----------------------------------------
      rawopt <- do
        if noRawOpt
        then return raw
        else do
          let opt = RawOpt.rawopt raw
          when verbose $ printSep  "OPTIMIZING RAW OPT"
          when verbose $ writeFileD "out/out.rawopt" (PP.render $ PPrint.runPP ppConfig $ Raw.ppProg opt)
          return opt

      ----- STACK ------------------------------------------
      let stack = Raw2Stack.rawProg2Stack rawopt
      when verbose $ printSep "GENERATING STACK"
      when verbose $ writeFileD "out/out.stack" (PP.render $ PPrint.runPP ppConfig $ Stack.ppProg stack)

      ----- JAVASCRIPT -------------------------------------
      -- The compiled artifact embeds its datatype records (spec §10): a library
      -- carries its own exported group hashes; any artifact that consumed
      -- imported datatypes carries the per-library consumed hashes.
      let records = Stack2JS.DatatypeRecords
                      { Stack2JS.drExported = map fst localGroups
                      , Stack2JS.drConsumed = consumedRecord }
      -- Record the project root and the dependencies file in the program when
      -- it (transitively) uses modules, so the runtime can seed its
      -- hash -> (location, name) resolver and locate compiled artifacts.
      let usesModules = let Prog (Imports imps) _ _ = prog
                        in any (\imp -> importPath imp /= Nothing) imps
          isProgram = usesModules && not (LibMode `elem` flags)
          moduleRoot     = if isProgram then Just root else Nothing
          moduleDepsFile = if isProgram then Just (depsFilePath (fromJust fname)) else Nothing
      let (stackjs, mappings) = Stack2JS.stack2JSWithMappings compileMode
                                                              debugJS
                                                              sourceMapEnabled
                                                              records
                                                              moduleRoot
                                                              moduleDepsFile
                                                              (Stack.ProgramStackUnit stack)

      ----- SOURCE MAP EMBEDDING ---------------------------
      -- When source maps are enabled, replace the placeholder with actual source map JSON.
      -- Also append the inline source map comment for Node.js --enable-source-maps compatibility.
      let finalJs = if sourceMapEnabled
                    then let mapJson = buildSourceMap outPath mappings
                             mapJsonStr = BSLazyChar8.unpack (Aeson.encode mapJson)
                             -- Replace placeholder with actual source map JSON using Data.Text.replace
                             jsWithMap = T.unpack $ T.replace
                                           (T.pack Stack2JS.sourceMapPlaceholderStr)
                                           (T.pack mapJsonStr)
                                           (T.pack stackjs)
                             -- Also add inline comment for backwards compatibility
                             mapBytes = BL.toStrict (Aeson.encode mapJson)
                             mapBase64 = B64.encode mapBytes
                             inlineComment = "\n//# sourceMappingURL=data:application/json;charset=utf-8;base64,"
                                             ++ BS.unpack mapBase64 ++ "\n"
                         in jsWithMap ++ inlineComment
                    else stackjs
      -- Atomic: a module artifact's .js may be read by another program's
      -- runtime while a parallel build rewrites it.
      atomicWriteFileD outPath finalJs

      -- A program-module artifact records its own content hash in its
      -- @.exports@ (the identity its consumers pick up); a stdlib library
      -- carries no such line.
      let moduleHashVal = if ModuleArtifact `elem` flags
                          then Just (ModuleHash.moduleHash iropt)
                          else Nothing
      case exports of
        Nothing -> return ()
        Just es -> do
          -- Every exported operator carries its fixity into the interface;
          -- a symbolic export without a fixity in the file's environment
          -- (local declarations plus unqualified imports, so re-export
          -- propagates) is an error.
          fixEnv <- case runExcept (fileFixityEnv sprog) of
                      Right m -> return m
                      Left s  -> die s
          let opNames = filter isOperatorName es
          exportedFixities <-
            mapM (\v -> case Map.lookup v fixEnv of
                          Just f  -> return (v, f)
                          Nothing -> die $ "exported operator '" ++ v
                                     ++ "' needs a fixity declaration in this file's header")
                 opNames
          writeExports outPath
            (exportsFileContent moduleHashVal es localGroups exportedFixities)

      ----- EPILOGUE --------------------------------------
      when verbose printHr
      return (resolvedDeps, ExitSuccess)

isOutputFile :: Flag -> Bool
isOutputFile (OutputFile _) = True
isOutputFile _              = False

--------------------------------------------------------------------------------
----- INGEST: troupe-ir-sexp file -> whole-program backend -> JS ---------------
-- Reads an s-expression IR file, parses it to an IRProgram, and runs the normal
-- whole-program backend (prog2raw -> rawopt -> raw2Stack -> stack2JS). No
-- ambient methods are injected: the ingested program must be self-contained.
ingestIRSexp :: [Flag] -> String -> String -> IO ExitCode
ingestIRSexp flags file input =
  case IRSexp.parseProg input of
    Left err -> die ("troupec: troupe-ir-sexp parse error: " ++ err)
    Right ir -> do
      let outPath  = outFile flags file
          noRawOpt = NoRawOpt `elem` flags
          debugJS  = Debug `elem` flags
          raw      = IR2Raw.prog2raw ir
          rawopt   = if noRawOpt then raw else RawOpt.rawopt raw
          stack    = Raw2Stack.rawProg2Stack rawopt
          (stackjs, _mappings) =
            Stack2JS.stack2JSWithMappings CompileMode.Normal debugJS False
                                          Stack2JS.noDatatypeRecords
                                          Nothing
                                          Nothing
                                          (Stack.ProgramStackUnit stack)
      writeFile outPath stackjs
      exitSuccess

--------------------------------------------------------------------------------
----- DEPENDENCIES FILE: enforce (normal compile) and establish (utility) -------

-- | Load the pins a program compile enforces against. An absent dependencies
-- file yields no pins (each module import then reports its own missing pin, a
-- compile error pointing at the utility); a malformed file is fatal here.
loadPinsIfPresent :: FilePath -> IO [DepEntry]
loadPinsIfPresent path = do
  r <- readDepsFile path
  case r of
    Nothing         -> return []
    Just (Left err) -> die $ "malformed dependencies file " ++ path ++ ": " ++ err
    Just (Right es) -> return es

-- | The maintenance utility: (re)establish the program's dependencies file.
-- Runs the same dependencies-first resolution and hashing as a normal compile
-- but in 'Establish' mode (recording actual hashes rather than checking pins),
-- then writes @<main>.deps.json@ and reports which pins moved.
updateDeps :: [Flag] -> FilePath -> String -> IO ExitCode
updateDeps o file input = do
  when (LibMode `elem` o) $ die "--update-deps applies to a program, not a library"
  let root        = takeDirectory file
      moduleFlags = LibMode : ModuleArtifact : filter (`elem` [NoRawOpt, Debug]) o
  mods <- discoverModules file
  -- Deps-first compiles refresh each module's .exports (with its current hash)
  -- and yield the actuals each consumer resolved.
  moduleDeps <- mapM (\m -> do createDirectoryIfMissing True (takeDirectory m </> "out")
                               minput <- readFile m
                               (ds, _) <- process Establish root moduleFlags (Just m) minput
                               return ds) mods
  -- Resolve the main file's own direct imports too. Direct the main compile's
  -- JS to the program's out/ dir (created here) so the utility is independent of
  -- the working directory (the default output is out/out.stack.js under CWD).
  let mainOut = takeDirectory file </> "out" </> takeBaseName file <.> "js"
  createDirectoryIfMissing True (takeDirectory mainOut)
  (mainDeps, _) <- process Establish root (OutputFile mainOut : o) (Just file) input
  let entries  = dedupByPath (concat moduleDeps ++ mainDeps)
      depsPath = depsFilePath file
  prev <- readDepsFile depsPath
  let prevEntries = case prev of Just (Right es) -> es; _ -> []
  reportPinChanges prevEntries entries
  writeDepsFile depsPath entries
  putStrLn $ "wrote " ++ show (length entries) ++ " module pin(s) to " ++ depsPath
  return ExitSuccess

-- | Keep the first entry per path (all occurrences of a path resolve to the
-- same hash — the module is compiled once — so first-wins is deterministic).
dedupByPath :: [DepEntry] -> [DepEntry]
dedupByPath = List.nubBy ((==) `on` depPath)

-- | Report added, changed, and removed pins relative to the previous file.
reportPinChanges :: [DepEntry] -> [DepEntry] -> IO ()
reportPinChanges prev new = do
  mapM_ report new
  mapM_ reportRemoved prev
  where
    report e = case lookupPinByPath (depPath e) prev of
      Nothing -> putStrLn $ "  + " ++ depPath e ++ "  " ++ depHash e ++ " (new)"
      Just p
        | depHash p == depHash e -> return ()
        | otherwise -> putStrLn $ "  ~ " ++ depPath e ++ "  " ++ depHash p ++ " -> " ++ depHash e
    reportRemoved e =
      case lookupPinByPath (depPath e) new of
        Just _  -> return ()
        Nothing -> putStrLn $ "  - " ++ depPath e ++ " (removed)"

-- TODO: 'where' for all helper functions below?
outFile :: [Flag] -> String -> String
outFile flags fname = case List.find isOutFlag flags of
                          Just (OutputFile s) -> s
                          _ -> if LibMode `elem` flags
                               then defaultName fname ++ ".js"
                               else "out/out.stack.js"
  where isOutFlag (OutputFile _) = True
        isOutFlag _              = False

        defaultName f = concat [ takeDirectory f
                               ,  "/out/"
                               , if takeExtension f == ".trp" then takeBaseName f else takeFileName f
                               ]

-- | Write the assembled @.exports@ interface content to the artifact's
-- @.exports@ file (dropping a trailing @.js@ from the output path).
writeExports :: FilePath -> String -> IO ()
writeExports path content =
  let path' = if takeExtension path == ".js" then dropExtension path else path
  in atomicWriteFileD (path' ++ ".exports") content

-- Utility functions for printing things out
hrWidth = 70

printSep :: String -> IO ()
printSep s = do
  let prefix = replicate 5 '-'
      suffix = replicate (hrWidth - length s - 5 - 2) '-'
      s' = prefix ++ " " ++ s ++ " " ++ suffix
  putStrLn s'


printHr :: IO ()
printHr = putStrLn (replicate hrWidth '-')

--------------------------------------------------------------------------------
----- DESERIALIZATION FOR INTERACTIVE MODES ------------------------------------

fromStdinIR putStrLn format = do
  eof <- isEOF
  if eof then exitSuccess else do
    input <- BS.getLine
    let echo = "!ECHO "
    if BS.isPrefixOf echo input
    then let response = BS.drop (BS.length echo) input
          in do BS.putStrLn response
    else
      case B64.decode input of
        Right bs ->
           case CCIR.deserialize bs
              of Right x -> do (putStrLn . format . ir2Stack) x
                 Left s -> do putStrLn "ERROR in deserialization"
                              debugOut $ "deserialization error" ++ s
        Left s -> do putStrLn "ERROR in B64 decoding"
                     debugOut $ "decoding error" ++s
    putStrLn "" -- magic marker to be recognized by the JS runtime; 2018-03-04; aa
    hFlush stdout
    fromStdinIR putStrLn format
  -- AA: 2018-07-15: consider timestamping these entries
  where debugOut s = appendFile "/tmp/debug" (s ++ "\n")

        ir2Stack = Raw2Stack.raw2Stack . RawOpt.rawopt . IR2Raw.ir2raw

fromStdinTextIR =
  let format = Stack2JS.stack2JSString CompileMode.Normal False
  in fromStdinIR putStrLn format

fromStdinJsonIR =
  let putStrLn = BSLazyChar8.putStrLn
      format   = Stack2JS.stack2JSON CompileMode.Normal False
  in fromStdinIR putStrLn format

--------------------------------------------------------------------------------
----- MAIN ---------------------------------------------------------------------

main :: IO ExitCode
main = do
  args <- getArgs
  case getOpt Permute options args of

-- AA: 2018-07-15: disabling REPL as it is pretty useless for now
--    ([],[],[]) -> repl

    ([Help], [], []) -> do
      putStrLn compilerUsage
      exitSuccess

    ([TextIRMode], [], []) -> fromStdinTextIR
    ([JSONIRMode], [], []) -> fromStdinJsonIR

    (o, [file], []) | optionsOK o -> do
      input <- readFile file
      if IngestIRSexp `elem` o
        then ingestIRSexp o file input
        else if UpdateDeps `elem` o
        then updateDeps o file input
        else do
          let root = takeDirectory file
          -- Resolve the module graph and load the pins the compile enforces
          -- against. A stdlib library compile (-l) has no module graph; a
          -- program that uses modules must have a dependencies file (a hard
          -- error otherwise, pointing at --update-deps).
          (pin, mods) <-
            if LibMode `elem` o
              then return (Enforce [], [])
              else do
                mods <- discoverModules file
                pins <- loadPinsIfPresent (depsFilePath file)
                return (Enforce pins, mods)
          -- Compile the module import graph first (dependencies before
          -- consumers), then the file itself. Modules compile as content-hashed
          -- library artifacts (LibMode + ModuleArtifact); per-module dumps are
          -- not written (Verbose stays top-level only).
          let moduleFlags = LibMode : ModuleArtifact : filter (`elem` [NoRawOpt, Debug]) o
          mapM_ (\m -> do createDirectoryIfMissing True (takeDirectory m </> "out")
                          minput <- readFile m
                          _ <- process pin root moduleFlags (Just m) minput
                          return ()) mods
          (_, ec) <- process pin root o (Just file) input
          return ec

    (_,_, errs) -> die $ concat errs ++ compilerUsage
 where
   compilerUsage = usageInfo header options
     where header = "Usage: <compiler> [OPTION...] file"

   -- Check options for consistency
   optionsOK :: [Flag] -> Bool
   optionsOK o | length o >=2 =
                -- certain options must not be combined
                not.or $ map (`elem` o) [TextIRMode, Help]
   optionsOK _ = True
