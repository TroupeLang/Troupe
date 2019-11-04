import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsStringDiff,  goldenVsString, findByExtension)
import System.Directory
import System.Process
import System.Exit 
import System.FilePath (takeBaseName, replaceExtension)
import qualified Data.ByteString.Lazy as LBS 
import qualified Data.ByteString.Char8
import System.Info 
import System.Environment 
-- import qualified System.IO.Strict

getOptionalInput :: String -> IO String 
getOptionalInput testfile = do 
    inputExists <- doesFileExist $ testfile ++ ".input"
    if inputExists then do 
        s <- readFile (testfile ++ ".input")
        return s 
    else 
        return ""        



runLocal testname = do 
    input <- getOptionalInput testname 
    readProcessWithExitCode "./local.sh" [testname] input

-- We use this to test the commands with timeouts.
-- Observe the current value for the timeout is 2 seconds.

runTimeout n testname = do 
    let timeout = if os == "darwin" then "gtimeout" else "timeout"        
    readProcessWithExitCode timeout [show n, "./local.sh", testname] ""


runPositiveTimeout :: Int -> String -> IO LBS.ByteString 
runPositiveTimeout t testname = do 
    (code, out, err) <- runTimeout t testname 
    case code of 
        ExitFailure _ -> return $ (LBS.fromStrict . Data.ByteString.Char8.pack) (out ++ err)
        ExitSuccess -> fail testname 
        


runPositive :: String -> IO LBS.ByteString 
runPositive testname = do 
    (code, out, err) <- runLocal testname 
    case code of 
        ExitSuccess -> return $ (LBS.fromStrict . Data.ByteString.Char8.pack) out
        ExitFailure _ -> fail testname 


runNegative :: String -> IO LBS.ByteString 
runNegative testname = do 
    (code, out, err) <- runLocal testname 
    case code of 
        ExitFailure _ -> return $ (LBS.fromStrict . Data.ByteString.Char8.pack) err
        ExitSuccess -> fail testname 
        

main :: IO () 
main = do 
         troupeDir <- getEnv "TROUPE"
         setCurrentDirectory troupeDir
         defaultMain =<< goldenTests 


goldenTests = do 
    let extensions =  [".trp", ".pico", ".atto", ".picox", ".femto"] 
    negativeTestsForCompiler <- findByExtension extensions "tests/cmp"
    positiveTestsForRuntime  <- findByExtension extensions "tests/rt/pos"
    negativeTestsForRuntime  <- findByExtension extensions "tests/rt/neg"
    warningTestsForRuntime   <- findByExtension extensions "tests/rt/warn"
    timeoutTestsForRuntime   <- findByExtension extensions "tests/rt/timeout/blocking"
    divergingTestsForRuntime <- findByExtension extensions "tests/rt/timeout/diverging"

    return $ (testGroup "Troupe golden tests" 
                                [ compilerTests negativeTestsForCompiler
                                , runtimeTests $ concat [positiveTestsForRuntime, negativeTestsForRuntime, warningTestsForRuntime]
                                , timeoutTests timeoutTestsForRuntime
                                , divergingTests divergingTestsForRuntime ] )


compilerTests testFiles =
    testGroup "Compiler (negative) tests"
        [goldenVsString 
            troupeFile 
            goldenFile 
            (runNegative troupeFile) 
        | troupeFile <- testFiles 
        , let goldenFile = replaceExtension troupeFile ".golden"
        ]

-- OBS: 2019-03-02: we are using a diff wrapper because the library used by
-- tasty-golden for starting a subprocess escapes quotes, making it impossible
-- to pass the regex arguments to diff, which is what we use to ignore logging
-- (through timestamps) and uuids when diffing. 

diff ref new = ["tests/_util/diff.sh", ref, new ]

diff_n ref new = ["tests/_util/diff_n.sh", ref, new ]


-- 2019-03-04: AA: we should probably use type classes... 

runtimeTests testFiles =
    testGroup "Runtime (positive, negative, and warnings) tests" 
        [ goldenVsStringDiff  
            troupeFile
            diff 
            goldenFile 
            (runPositive troupeFile) 
        | troupeFile <- testFiles 
        , let goldenFile = replaceExtension troupeFile ".golden"
        ] 


timeoutTests testFiles = 
    testGroup "Timeout tests" 
        [ goldenVsStringDiff  
            troupeFile            
            diff 
            goldenFile 
            (runPositiveTimeout 4 troupeFile) 
        | troupeFile <- testFiles 
        , let goldenFile = replaceExtension troupeFile ".golden"
        ] 


divergingTests testFiles = 
    testGroup "Diverging tests" 
        [ goldenVsStringDiff  
            troupeFile            
            diff_n
            goldenFile 
            (runPositiveTimeout 2 troupeFile)
        | troupeFile <- testFiles 
        , let goldenFile = replaceExtension troupeFile ".golden"
        ] 

        