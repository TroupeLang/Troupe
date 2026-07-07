{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Test.Tasty (defaultMain, TestTree, testGroup, defaultMainWithIngredients, defaultIngredients, askOption, includingOptions)
import Test.Tasty.Golden (goldenVsStringDiff,  goldenVsString, findByExtension)
import Test.Tasty.Options (IsOption(..), OptionDescription(..), safeRead, flagCLParser)
import Data.Typeable (Typeable)
import Data.Tagged
import Data.Proxy
import Options.Applicative
import System.Directory
import System.Process
import System.Exit 
import System.FilePath (takeBaseName, replaceExtension, takeDirectory)
import qualified Data.ByteString.Lazy as LBS 
import qualified Data.ByteString.Char8
import System.Info
import System.Environment (getEnv, getArgs, lookupEnv, getExecutablePath)
import Data.List (isSuffixOf)
import qualified ShellWords
-- import qualified System.IO.Strict

-- When having multiple optimizations / optional compiler stages or
-- other flags changing the output, probably want to generate all combinations
-- and run the tests on them.
data TestConfig = TestConfig 
    { tcRawOpt :: Bool
    , tcNoColor :: Bool 
    }

-- Custom option for no-color mode
newtype NoColorOption = NoColorOption Bool
  deriving (Eq, Ord, Typeable)

instance IsOption NoColorOption where
  defaultValue = NoColorOption False
  parseValue = fmap NoColorOption . safeRead
  optionName = return "no-color"
  optionHelp = return "Disable colored output (generates .nocolor.golden files)"
  optionCLParser = flagCLParser Nothing (NoColorOption True)

-- Custom option for quick mode (skip unoptimized tests)
newtype QuickOption = QuickOption Bool
  deriving (Eq, Ord, Typeable)

instance IsOption QuickOption where
  defaultValue = QuickOption False
  parseValue = fmap QuickOption . safeRead
  optionName = return "quick"
  optionHelp = return "Run only optimized tests (skip --no-rawopt pass)"
  optionCLParser = flagCLParser Nothing (QuickOption True)


ppTestConfig TestConfig{..} =
    (if tcRawOpt then "Raw optimized" else "Raw NOT optimized") ++
    (if tcNoColor then ", No color" else ", With color")


getOptionalInput :: String -> IO String
getOptionalInput testfile = do
    inputExists <- doesFileExist $ testfile ++ ".input"
    if inputExists then do
        s <- readFile (testfile ++ ".input")
        return s
    else
        return ""

-- | Read optional per-test options from a .options file
-- Uses ShellWords to parse shell-style arguments properly
getOptionalOptions :: String -> IO [String]
getOptionalOptions testfile = do
    let optionsFile = testfile ++ ".options"
    optionsExists <- doesFileExist optionsFile
    if optionsExists then do
        content <- readFile optionsFile
        let filtered = unlines $ filter notComment $ lines content
        case ShellWords.parse filtered of
            Right args -> return args
            Left _     -> return []  -- on parse error, return empty
    else
        return []
  where notComment ('#':_) = False; notComment _ = True


goldenFileName :: String -> TestConfig -> String
goldenFileName troupeFile TestConfig{..} =
    if tcNoColor
    then replaceExtension troupeFile ".nocolor.golden"
    else replaceExtension troupeFile ".golden"

mkRunArgs :: TestConfig -> [String]
mkRunArgs TestConfig{..} =
    (if tcRawOpt then [] else ["--no-rawopt"]) ++
    (if tcNoColor then ["--no-color"] else [])

-- | Build the full argument list for ./local.sh
mkLocalArgs :: String -> TestConfig -> IO [String]
mkLocalArgs testname tc = do
    extraArgs <- getOptionalOptions testname
    return $ mkRunArgs tc ++ [testname] ++ extraArgs

runLocal :: String -> TestConfig -> IO (ExitCode, String, String)
runLocal testname tc = do
    input <- getOptionalInput testname
    args <- mkLocalArgs testname tc
    readProcessWithExitCode "./local.sh" args input

-- We use this to test the commands with timeouts.
-- Observe the current value for the timeout is 2 seconds.

-- | GNU timeout command used by the timeout and diverging test groups.
-- On macOS the stock system has no `timeout`; `gtimeout` comes from
-- GNU coreutils.
timeoutCommand :: String
timeoutCommand = if os == "darwin" then "gtimeout" else "timeout"

-- | Fail at startup with an actionable message when the timeout
-- command is missing, instead of a command-not-found error per test.
checkTimeoutCommand :: IO ()
checkTimeoutCommand = do
    found <- findExecutable timeoutCommand
    case found of
        Just _  -> return ()
        Nothing -> die $ unlines $
            [ "Cannot find '" ++ timeoutCommand ++ "' on PATH."
            , "The timeout and diverging test groups require it." ] ++
            [ "Install GNU coreutils: brew install coreutils" | os == "darwin" ]

runTimeout :: Int -> String -> TestConfig -> IO (ExitCode, String, String)
runTimeout n testname tc = do
    args <- mkLocalArgs testname tc
    readProcessWithExitCode timeoutCommand ([show n, "./local.sh"] ++ args) ""


runPositiveTimeout :: Int -> String -> TestConfig -> IO LBS.ByteString
runPositiveTimeout t testname tc = do
    (code, out, err) <- runTimeout t testname tc
    case code of 
        ExitFailure _ -> return $ (LBS.fromStrict . Data.ByteString.Char8.pack) (out ++ err)
        ExitSuccess -> fail testname 
        


runPositive :: String -> TestConfig -> IO LBS.ByteString
runPositive testname tc = do
    (code, out, err) <- runLocal testname tc
    case code of 
        ExitSuccess -> return $ (LBS.fromStrict . Data.ByteString.Char8.pack) out
        ExitFailure _ -> fail testname 


runNegative :: String -> TestConfig -> IO LBS.ByteString
runNegative testname tc = do
    (code, out, err) <- runLocal testname tc
    case code of 
        ExitFailure _ -> return $ (LBS.fromStrict . Data.ByteString.Char8.pack) err
        ExitSuccess -> fail testname 
        

-- | Search upward from a directory for the .troupe-root marker file.
-- This supports git worktrees where multiple checkouts share the same
-- repository structure but have different root directories.
-- Returns Nothing if we reach the filesystem root without finding the marker.
findTroupeRootUpward :: FilePath -> IO (Maybe FilePath)
findTroupeRootUpward dir = do
    let markerPath = dir ++ "/.troupe-root"
    exists <- doesFileExist markerPath
    if exists
    then return (Just dir)
    else do
        let parent = takeDirectory dir
        if parent == dir  -- reached filesystem root (e.g., "/" on Unix)
        then return Nothing
        else findTroupeRootUpward parent

-- | Determine the Troupe root directory using multiple strategies:
--
-- 1. **Executable path check**: If running as the installed `bin/golden` binary,
--    strip the `/bin/golden` suffix to get the root. This is the fastest path
--    for the common case of running the installed binary directly.
--
-- 2. **Current directory search**: Search upward from the current working
--    directory for a `.troupe-root` marker file. This supports:
--    - Running via `stack test` (executable is in .stack-work/dist/...)
--    - Git worktrees where each worktree has its own .troupe-root marker
--    - Running from any subdirectory of a Troupe checkout
--
-- 3. **Environment variable fallback**: Use the TROUPE environment variable
--    if set. This is the legacy method and serves as a final fallback.
--
-- The .troupe-root file is an empty marker file that should exist at the
-- root of each Troupe checkout or worktree.
getTroupeRoot :: IO String
getTroupeRoot = do
    progPath <- getExecutablePath
    let binSuffix = "/bin/golden"
    if binSuffix `isSuffixOf` progPath
    then do
        -- Running as installed bin/golden binary
        let home = take (length progPath - length binSuffix) progPath
        markerExists <- doesFileExist (home ++ "/.troupe-root")
        if markerExists then return home else searchFromCwd
    else searchFromCwd
  where
    -- Search upward from current working directory for .troupe-root
    -- This handles `stack test` and git worktrees
    searchFromCwd = do
        cwd <- getCurrentDirectory
        result <- findTroupeRootUpward cwd
        case result of
            Just root -> return root
            Nothing -> fallbackToEnv
    -- Final fallback: check TROUPE environment variable
    fallbackToEnv = do
        maybeEnv <- lookupEnv "TROUPE"
        case maybeEnv of
            Just troupeDir -> return troupeDir
            Nothing -> error $ unlines
                [ "Cannot determine Troupe root directory."
                , "Tried:"
                , "  1. Executable path (looking for /bin/golden suffix)"
                , "  2. Searching upward from cwd for .troupe-root marker"
                , "  3. TROUPE environment variable"
                , ""
                , "Solutions:"
                , "  - Run from within a Troupe checkout with .troupe-root file"
                , "  - Set the TROUPE environment variable to the Troupe root"
                ]

main :: IO ()
main = do
    troupeDir <- getTroupeRoot
    setCurrentDirectory troupeDir
    checkTimeoutCommand

    -- Pre-generate test configurations (full and quick variants)
    testsWithColorFull <- sequence
      [ goldenTests (TestConfig True False)   -- Raw opt, with color
      , goldenTests (TestConfig False False)  -- No raw opt, with color
      ]
    testsWithColorQuick <- sequence
      [ goldenTests (TestConfig True False)   -- Raw opt only, with color
      ]
    testsNoColorFull <- sequence
      [ goldenTests (TestConfig True True)    -- Raw opt, no color
      , goldenTests (TestConfig False True)   -- No raw opt, no color
      ]
    testsNoColorQuick <- sequence
      [ goldenTests (TestConfig True True)    -- Raw opt only, no color
      ]

    defaultMainWithIngredients ings $
      askOption $ \(NoColorOption noColor) ->
      askOption $ \(QuickOption quick) ->
        testGroup "Troupe golden tests" $
          case (noColor, quick) of
            (False, False) -> testsWithColorFull
            (False, True)  -> testsWithColorQuick
            (True, False)  -> testsNoColorFull
            (True, True)   -> testsNoColorQuick
  where
    ings = includingOptions [Option (Proxy :: Proxy NoColorOption), Option (Proxy :: Proxy QuickOption)] : defaultIngredients


goldenTests :: TestConfig -> IO TestTree
goldenTests tc = do
    let extensions = [".trp"]
    negativeTestsForCompiler <- findByExtension extensions "tests/cmp"
    positiveTestsForRuntime  <- findByExtension extensions "tests/rt/pos"
    negativeTestsForRuntime  <- findByExtension extensions "tests/rt/neg"
    warningTestsForRuntime   <- findByExtension extensions "tests/rt/warn"
    timeoutTestsForRuntime   <- findByExtension extensions "tests/rt/timeout/blocking"
    divergingTestsForRuntime <- findByExtension extensions "tests/rt/timeout/diverging"
    testsForLib              <- findByExtension extensions "tests/lib"

    return $ (testGroup ("Troupe golden tests (" ++ ppTestConfig tc ++ ")") $ map ($ tc)
                                [ compilerTests negativeTestsForCompiler
                                , runtimeTests $ concat [positiveTestsForRuntime, negativeTestsForRuntime, warningTestsForRuntime]
                                , timeoutTests timeoutTestsForRuntime
                                , divergingTests divergingTestsForRuntime
                                , libTests testsForLib] )


compilerTests testFiles tc =
    testGroup "Compiler (negative) tests"
        [goldenVsString 
            troupeFile 
            (goldenFileName troupeFile tc)
            (runNegative troupeFile tc)
        | troupeFile <- testFiles 
        ]

-- OBS: 2019-03-02: we are using a diff wrapper because the library used by
-- tasty-golden for starting a subprocess escapes quotes, making it impossible
-- to pass the regex arguments to diff, which is what we use to ignore logging
-- (through timestamps) and uuids when diffing. 

diff ref new = ["tests/_util/diff.sh", ref, new ]

diff_n ref new = ["tests/_util/diff_n.sh", ref, new ]


-- 2019-03-04: AA: we should probably use type classes... 

runtimeTests testFiles tc =
    testGroup "Runtime tests" 
        [ goldenVsStringDiff  
            troupeFile
            diff 
            (goldenFileName troupeFile tc)
            (runPositive troupeFile tc)
        | troupeFile <- testFiles 
        ] 


timeoutTests testFiles tc =
    testGroup "Timeout tests" 
        [ goldenVsStringDiff  
            troupeFile            
            diff 
            (goldenFileName troupeFile tc)
            (runPositiveTimeout 8 troupeFile tc)
        | troupeFile <- testFiles 
        ] 


divergingTests testFiles tc =
    testGroup "Diverging tests" 
        [ goldenVsStringDiff  
            troupeFile            
            diff_n
            (goldenFileName troupeFile tc)
            (runPositiveTimeout 8 troupeFile tc)
        | troupeFile <- testFiles 
        ]

libTests testFiles tc =
    testGroup "Library tests"
        [ goldenVsStringDiff
            troupeFile
            diff
            (goldenFileName troupeFile tc)
            (runPositive troupeFile tc)
        | troupeFile <- testFiles
        ]

