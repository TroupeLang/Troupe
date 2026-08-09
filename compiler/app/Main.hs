{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Environment
import Util.FileUtil
import qualified IR as CCIR
-- import qualified RetRewrite as Rewrite
import qualified IR2Raw
import qualified IRSexp
import qualified IRBlob
import qualified Pipeline
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
import DepsFile (DepEntry(..), depsFilePath, readDepsFile, writeDepsFile, lookupPinByPath)
import Direct (Prog(..))
import Basics (Imports(..), ImportSource(..), importSource)
import System.Directory (createDirectoryIfMissing)
import Exports
import CompileMode
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
  , Option []    ["ingest-ir-sexp"] (NoArg IngestIRSexp) "read a troupe-ir-sexp file and compile it to JS (the program must be self-contained)"
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
  let verbose = Verbose `elem` flags
      copts    = compileOpts flags

  do
      let outPath = outFile flags (fromJust fname)

      ------------------------------------------------------
      -- TROUPE (FRONTEND) ---------------------------------
      folded <- Pipeline.frontEndFold pin root copts (maybe "" id fname) input
      let sprog          = Pipeline.fdSurface folded
          prog           = Pipeline.fdProg folded
          localGroups    = Pipeline.fdLocal folded      -- [(group hash, canonical form)]
          consumedRecord = Pipeline.fdConsumed folded   -- [(library, consumed hashes)]
          exports        = Pipeline.fdExports folded
          resolvedDeps   = Pipeline.fdDeps folded

      when (DatatypeHashes `elem` flags) $ do
        putStr (datatypeHashReport localGroups)
        exitSuccess

      ------------------------------------------------------
      ------ IR (BACKEND) ----------------------------------
      iropt <- Pipeline.frontEndIR copts folded

      ------ EMIT troupe-ir-sexp (and stop) ----------------
      when (EmitIRSexp `elem` flags) $ do
        let sexp = IRSexp.printProgWithPos iropt
        case List.find isOutputFile flags of
          Just (OutputFile f) -> writeFile f sexp
          _                   -> putStr sexp
        exitSuccess

      emitJS outPath (maybe "" id fname) root flags copts folded iropt

      Pipeline.writeUnitExports outPath (ModuleArtifact `elem` flags) folded iropt

      ----- EPILOGUE --------------------------------------
      when verbose printHr
      return (resolvedDeps, ExitSuccess)

-- | Flags to pipeline options. Stage dumps are the executable's business: the
-- pipeline hands them out, this decides they become files under out/ (and
-- nothing at all unless -v).
compileOpts :: [Flag] -> Pipeline.CompileOpts
compileOpts flags =
  Pipeline.CompileOpts { Pipeline.coMode       = if LibMode `elem` flags then Library else Normal
                       , Pipeline.coDump       = dump
                       , Pipeline.coPPConfig   = ppConfig }
  where
    dump | Verbose `elem` flags = Pipeline.StageDump { Pipeline.dumpSep  = printSep
                                                     , Pipeline.dumpFile = writeFileD
                                                     , Pipeline.dumpEcho = putStrLn }
         | otherwise            = Pipeline.silentDump
    ppConfig = PPrint.mkPPConfig (DebugPP `elem` flags) (PPrint.parsePosFormat ppPosFormatStr)
    isPPPosFormatFlag (PPPosFormat _) = True
    isPPPosFormatFlag _               = False
    ppPosFormatStr = case List.find isPPPosFormatFlag flags of
                       Just (PPPosFormat s) -> s
                       _                    -> "inline"

-- | IR to JavaScript on disk: Raw, Raw optimization, stack layout, emission,
-- source map. Used for the main compilation unit and for each module in a
-- program's import graph.
emitJS :: FilePath -> FilePath -> FilePath -> [Flag] -> Pipeline.CompileOpts
       -> Pipeline.Folded -> CCIR.IRProgram -> IO ()
emitJS outPath srcPath root flags copts folded iropt = do
  let verbose          = Verbose `elem` flags
      noRawOpt         = NoRawOpt `elem` flags
      debugJS          = Debug `elem` flags
      sourceMapEnabled = SourceMap `elem` flags
      compileMode      = Pipeline.coMode copts
      ppConfig         = Pipeline.coPPConfig copts

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
  -- The compiled artifact embeds its datatype records: a library carries its
  -- own exported group hashes; any artifact that consumed imported datatypes
  -- carries the per-library consumed hashes.
  let records = Stack2JS.DatatypeRecords
                  { Stack2JS.drExported = map fst (Pipeline.fdLocal folded)
                  , Stack2JS.drConsumed = Pipeline.fdConsumed folded }
  -- Record the project root and the dependencies file in the program when it
  -- (transitively) uses modules, so the runtime can seed its
  -- hash -> (location, name) resolver and locate compiled artifacts.
  let usesModules = let Prog (Imports imps) _ _ = Pipeline.fdProg folded
                    in any (\imp -> case importSource imp of
                                       FromModule _ -> True
                                       _            -> False) imps
      isProgram = usesModules && not (LibMode `elem` flags)
      moduleRoot     = if isProgram then Just root else Nothing
      -- Keyed off the source, not the output: the dependencies file sits
      -- beside the program's .trp, while the output may be anywhere (-o, or a
      -- temporary path when a script compiles and runs in one step).
      moduleDepsFile = if isProgram then Just (depsFilePath srcPath) else Nothing
  let (stackjs, mappings) = Stack2JS.stack2JSWithMappings compileMode
                                                          debugJS
                                                          sourceMapEnabled
                                                          records
                                                          moduleRoot
                                                          moduleDepsFile
                                                          (Stack.ProgramStackUnit stack)

  ----- SOURCE MAP EMBEDDING ---------------------------
  -- When source maps are enabled, replace the placeholder with the source map
  -- JSON, and append the inline comment Node's --enable-source-maps reads.
  let finalJs = if sourceMapEnabled
                then let mapJson = buildSourceMap outPath mappings
                         mapJsonStr = BSLazyChar8.unpack (Aeson.encode mapJson)
                         jsWithMap = T.unpack $ T.replace
                                       (T.pack Stack2JS.sourceMapPlaceholderStr)
                                       (T.pack mapJsonStr)
                                       (T.pack stackjs)
                         mapBytes = BL.toStrict (Aeson.encode mapJson)
                         mapBase64 = B64.encode mapBytes
                         inlineComment = "\n//# sourceMappingURL=data:application/json;charset=utf-8;base64,"
                                         ++ BS.unpack mapBase64 ++ "\n"
                     in jsWithMap ++ inlineComment
                else stackjs
  -- Atomic: a module artifact's .js may be read by another program's runtime
  -- while a parallel build rewrites it.
  atomicWriteFileD outPath finalJs

isOutputFile :: Flag -> Bool
isOutputFile (OutputFile _) = True
isOutputFile _              = False

--------------------------------------------------------------------------------
----- INGEST: troupe-ir-sexp file -> whole-program backend -> JS ---------------
-- Reads an s-expression IR file, parses it to an IRProgram, and runs the normal
-- whole-program backend (prog2raw -> rawopt -> raw2Stack -> stack2JS). The
-- ingested program must be self-contained.
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
           case IRBlob.deserialize bs
              of Right x -> do (putStrLn . format . ir2Stack) x
                 Left s -> do putStrLn "ERROR in deserialization"
                              debugOut $ "deserialization error" ++ s
        Left s -> do putStrLn "ERROR in B64 decoding"
                     debugOut $ "decoding error" ++s
    putStrLn "" -- magic marker to be recognized by the JS runtime; 2018-03-04; aa
    hFlush stdout
    fromStdinIR putStrLn format
  -- Diagnostics go to stderr: stdout carries the protocol the runtime parses, and a
  -- file append needs a writable path the host may not have (under WASI, /tmp is not
  -- preopened unless the embedder says so, and the append throws rather than logging).
  where debugOut s = hPutStrLn stderr s

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
          _ <- pure mods
          Pipeline.compileModuleGraph pin root (compileOpts moduleFlags) file
            (\m folded iropt ->
               emitJS (Pipeline.moduleOutPath m) m root moduleFlags (compileOpts moduleFlags)
                      folded iropt)
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
