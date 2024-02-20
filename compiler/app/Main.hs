{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified AtomFolding as AF
import Parser
import qualified Core as Core
import RetDFCPS
import qualified CaseElimination as C
import System.Environment
import Util.FileUtil
import qualified ClosureConv as CC
import qualified IR as CCIR
-- import qualified IROpt
-- import qualified RetRewrite as Rewrite
import qualified CPSOpt as CPSOpt
import qualified IR2JS
import qualified IR2Raw
-- import qualified Stack 
import qualified Raw2Stack
import qualified Stack2JS
import qualified RawOpt
-- import System.IO (isEOF)
import qualified Data.ByteString as BS
import Data.ByteString.Base64 (decode)
import qualified Data.ByteString.Char8  as BSChar8
import qualified Data.ByteString.Lazy.Char8 as BSLazyChar8
import System.IO
import System.Exit
import ProcessImports
import AddAmbientMethods
import ShowIndent
import Exports
import CompileMode
import Control.Monad.Except
import System.Console.GetOpt
import Data.List as List
import Data.Maybe (fromJust)
import System.FilePath

-- import System.Console.Haskeline
-- import System.Process


-- compiler flags
--
data Flag
  = IRMode
  | JSONIRMode
  | LibMode
  | NoRawOpt
  | OutputFile String
  | Verbose
  | Help
  | Debug
  deriving (Show, Eq)

options :: [OptDescr Flag]
options =
  [ Option ['i']      ["ir"]     (NoArg IRMode)              "ir interactive mode"
  , Option ['j']      ["json"]   (NoArg JSONIRMode)          "ir json interactive mode"
  , Option []         ["no-rawopt"] (NoArg NoRawOpt) "disable Raw optimization"
  , Option ['v']      ["verbose"] (NoArg Verbose)            "verbose output"
  , Option ['d']      ["debug"]  (NoArg Debug)               "debugging information in the .js file"
  , Option ['l']      ["lib"]    (NoArg LibMode)             "compiling a library"
  , Option ['h']      ["help"]   (NoArg Help)                "print usage"
  , Option ['o']      ["output"] (ReqArg OutputFile "FILE")  "output FILE"
  ]



process :: [Flag] -> Maybe String -> String -> IO ExitCode
process flags fname input = do
--  let tokens = parseTokens input
  let ast    = parseProg input

  let compileMode =
        if elem LibMode flags then Export
        else Normal

  let verbose = Verbose `elem` flags
      noRawOpt = NoRawOpt `elem` flags

  case ast of
    Left err -> do
      -- putStrLn ("Tokens: " ++ show tokens)
      die $ "Parse Error:\n" ++ err

    Right prog_parsed -> do
      let prog_empty_imports = 
            case compileMode of 
                Normal -> addAmbientMethods prog_parsed 
                Export -> prog_parsed 
      prog <- processImports prog_empty_imports
      
      exports <- case compileMode of
        Normal -> return Nothing
        Export -> case runExcept (extractExports prog) of
          Right es -> return (Just (es))
          Left s -> die s
       

      when verbose $ do printSep "SYNTAX"
                        putStrLn (showIndent 2 prog)

      --------------------------------------------------
      let prog' = ((C.trans compileMode) . AF.visitProg) prog
      when verbose $ do printSep "PATTERN MATCH ELIMINATION"
                        writeFileD "out/out.nopats" (showIndent 2 prog')
      --------------------------------------------------
      let lowered = Core.lowerProg prog'
      when verbose $ do printSep  "LOWERING FUNS AND LETS"
                        writeFileD "out/out.lowered" (showIndent 2 lowered)
      --------------------------------------------------
      let renamed = Core.renameProg lowered
      when verbose $ do printSep "α RENAMING"
                        writeFileD "out/out.alpha" (showIndent 2 renamed)
      --------------------------------------------------
      let cpsed = RetDFCPS.transProg renamed
      when verbose $ do printSep "CPSED"
                        writeFileD "out/out.cps" (showIndent 2 cpsed)
      --------------------------------------------------
      let rwcps = CPSOpt.rewrite cpsed -- Rewrite.rewrite cpsed
      when verbose $ do printSep  "REWRITING CPS"
                        writeFileD "out/out.cpsopt" (showIndent 2 rwcps)
      --------------------------------------------------
      ir <- case runExcept (CC.closureConvert compileMode rwcps) of 
          Right ir -> return ir 
          Left  s -> die $ "troupec: " ++ s
        
      
      

      when verbose $ writeFileD "out/out.ir" (show ir)

      -- let iropt = IROpt.iropt ir 
      let iropt = ir 
      --when verbose $ writeFileD "out/out.iropt" (show iropt)
      

      --------------------------------------------------
      let debugOut = elem Debug flags 


      ------ RAW -----------------------------------------
      let raw = IR2Raw.prog2raw iropt
      when verbose $ printSep  "GENERATING RAW"
      when verbose $ writeFileD "out/out.rawout" (show raw)

      ----- RAW OPT --------------------------------------

      rawopt <- do
            if noRawOpt
            then return raw
            else do
              let opt = RawOpt.rawopt raw
              when verbose $ printSep  "OPTIMIZING RAW OPT"
              when verbose $ writeFileD "out/out.rawopt" (show opt)
              return opt

      ----- STACK ----------------------------------------
      let stack = Raw2Stack.rawProg2Stack rawopt
      when verbose $ printSep "GENARTING STACK"
      when verbose $ writeFileD "out/out.stack" (show stack)
      let stackjs = Stack2JS.irProg2JSString compileMode debugOut stack
      let jsFile = outFile flags (fromJust fname)
      writeFile jsFile stackjs


      case exports of
        Nothing -> return ()
        Just es -> writeExports jsFile es
      when verbose printHr

      exitSuccess




writeExports jsF exports =
  let exF' = if takeExtension jsF == ".js" then dropExtension jsF else jsF
  in writeFileD (exF' ++ ".exports") (intercalate "\n" exports)



defaultName f =
   let ext = ".trp"
   in concat [ takeDirectory f
             ,  "/out/"
             , if takeExtension f == ext then takeBaseName f else takeFileName f
             ]


isOutFlag (OutputFile _) = True
isOutFlag _ = False

outFile :: [Flag] -> String -> String
outFile flags fname | LibMode `elem` flags =
                        case List.find isOutFlag flags of
                          Just (OutputFile s) -> s
                          _ -> defaultName fname ++ ".js"
outFile flags _ =
  case List.find isOutFlag flags of
    Just (OutputFile s) -> s
    _ -> "out/out.stack.js"


-- AA: 2018-07-15: consider timestamping these entries
debugOut s =
  appendFile "/tmp/debug" (s ++ "\n")


fromStdinIR = do
  eof <- isEOF
  if eof then exitSuccess else do
    input <- BS.getLine
    if BS.isPrefixOf "!ECHO " input
    then let response = BS.drop 6 input
          in do BSChar8.putStrLn response
--                  debugOut "echo"
    else
      case decode input of
        Right bs ->
           case CCIR.deserialize bs
              of Right x -> do putStrLn (IR2JS.irToJSString x)
--                                 debugOut "deserialization OK"

                 Left s -> do putStrLn "ERROR in deserialization"
                              debugOut $ "deserialization error" ++ s
        Left s -> do putStrLn "ERROR in B64 decoding"
                     debugOut $ "decoding error" ++s
    putStrLn "" -- magic marker to be recognized by the JS runtime; 2018-03-04; aa
    hFlush stdout
    fromStdinIR


fromStdinIRJson = do
  eof <- isEOF
  if eof then exitSuccess else do
    input <- BS.getLine
    if BS.isPrefixOf "!ECHO " input
    then let response = BS.drop 6 input
          in BSChar8.putStrLn response
    else
      case decode input of
        Right bs ->
           case CCIR.deserialize bs
              of Right x -> BSLazyChar8.putStrLn (IR2JS.irToJSON x)
                 Left s -> do putStrLn "ERROR in deserialization"
                              debugOut $ "deserialization error" ++ s
        Left s -> do putStrLn "ERROR in B64 decoding"
                     debugOut $ "decoding error" ++s
    putStrLn "" -- magic marker to be recognized by the JS runtime; 2018-03-04; aa
    hFlush stdout
    fromStdinIRJson

main :: IO ExitCode
main = do
  args <- getArgs
  case getOpt Permute options args of

-- AA: 2018-07-15: disabling REPL as it is pretty useless for now
--    ([],[],[]) -> repl

    ([Help], [], []) -> do
      putStrLn compilerUsage
      exitSuccess

    ([JSONIRMode], [], []) -> fromStdinIRJson   

    ([IRMode], [], []) -> do
      fromStdinIR
      -- hSetBuffering stdout NoBuffering

    (o, [file], []) | optionsOK o ->
      fromFile o file


    (_,_, errs) -> die $ concat errs ++ compilerUsage
 where
   compilerUsage = usageInfo header options
     where header = "Usage: <compiler> [OPTION...] file"


   -- Check options for consistency
   optionsOK :: [Flag] -> Bool
   optionsOK o | length o >=2 =
                -- certain options must not be combined
                not.or $ map (`elem` o) [IRMode, Help]
   optionsOK _ = True



fromFile :: [Flag] -> String -> IO ExitCode
fromFile flags fname = do
  input <- readFile fname
  process flags (Just fname) input


-- utility functions for printing things out

hrWidth = 70

printSep :: String -> IO ()
printSep s = do
  let prefix = replicate 5 '-'
      suffix = replicate (hrWidth - length s - 5 - 2) '-'
      s' = prefix ++ " " ++ s ++ " " ++ suffix
  putStrLn s'


printHr :: IO ()
printHr = putStrLn (replicate hrWidth '-')

--------------------------------------------------
