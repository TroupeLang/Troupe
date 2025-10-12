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
import System.FilePath (takeBaseName, replaceExtension)
import qualified Data.ByteString.Lazy as LBS 
import qualified Data.ByteString.Char8
import System.Info 
import System.Environment (getEnv, getArgs)
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


goldenFileName :: String -> TestConfig -> String
goldenFileName troupeFile TestConfig{..} = 
    if tcNoColor 
    then replaceExtension troupeFile ".nocolor.golden"
    else replaceExtension troupeFile ".golden"

mkRunArgs :: TestConfig -> [String]
mkRunArgs TestConfig{..} =
    (if tcRawOpt then [] else ["--no-rawopt"]) ++
    (if tcNoColor then ["--no-color"] else [])

runLocal :: String -> TestConfig -> IO (ExitCode, String, String)
runLocal testname tc = do
    input <- getOptionalInput testname
    readProcessWithExitCode "./local.sh" (mkRunArgs tc ++ [testname]) input

-- We use this to test the commands with timeouts.
-- Observe the current value for the timeout is 2 seconds.

runTimeout :: Int -> String -> TestConfig -> IO (ExitCode, String, String)
runTimeout n testname tc = do
    let timeout = if os == "darwin" then "gtimeout" else "timeout"
    readProcessWithExitCode timeout ([show n, "./local.sh"] ++ mkRunArgs tc ++ [testname]) ""


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
        

main :: IO () 
main = do
    troupeDir <- getEnv "TROUPE"
    setCurrentDirectory troupeDir
    
    -- Pre-generate all test configurations
    testsWithColor <- sequence
      [ goldenTests (TestConfig True False)   -- Raw opt, with color
      , goldenTests (TestConfig False False)  -- No raw opt, with color  
      ]
    testsNoColor <- sequence
      [ goldenTests (TestConfig True True)    -- Raw opt, no color
      , goldenTests (TestConfig False True)   -- No raw opt, no color
      ]
    
    defaultMainWithIngredients ings $
      askOption $ \(NoColorOption noColor) ->
        testGroup "Troupe golden tests" $
          if noColor then testsNoColor else testsWithColor
  where
    ings = includingOptions [Option (Proxy :: Proxy NoColorOption)] : defaultIngredients


goldenTests :: TestConfig -> IO TestTree
goldenTests tc = do
    let extensions =  [".trp", ".pico", ".atto", ".picox", ".femto"] 
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

