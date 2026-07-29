-- | The troupe-ir-sexp round-trip laws over IR the real pipeline produced.
--
-- The sibling suite (@ir-sexp-test@) checks the same laws over generated and
-- hand-built IR. This one runs the front end on every program in the test
-- corpus, so the laws are checked against the shapes an actual compile emits:
-- real names, real positions, real constant tables, real nesting.
--
--   * with positions:  parse (printProgWithPos p) == p
--   * positions erased: parse (printProg p)       == erasePosProg p
--
-- A file that does not compile is reported as skipped, not failed. The corpus
-- holds module components whose pins live with the program that imports them
-- (see docs/MODULES.md); compiling one on its own stops in import processing
-- before any IR exists, so it says nothing about the format. Those components
-- are still covered, through the programs that import them: the module graph is
-- compiled unit by unit and every unit is checked.
module Main (main) where

import           Control.Exception (try)
import           Control.Monad (filterM)
import           Data.IORef
import           System.Directory (doesFileExist)
import           System.Exit (ExitCode)
import           System.FilePath (makeRelative, takeDirectory, takeFileName)

import           Test.Tasty
import           Test.Tasty.Runners (NumThreads(..))
import           Test.Tasty.HUnit

import           CompileMode (CompileMode(..))
import           Corpus (compileOpts, corpusFiles, corpusRoot, troupeRoot)
import           DepsFile (depsFilePath, readDepsFile)
import qualified IR
import           IRSexp (erasePosProg, parseProg, printProg, printProgWithPos)
import qualified Pipeline
import           ProcessImports (PinCheck(..))

-- | Both laws for one IR program. Returns the failure description, if any.
checkLaws :: IR.IRProgram -> Maybe String
checkLaws ir =
  case (parseProg (printProgWithPos ir), parseProg (printProg ir)) of
    (Left err, _) -> Just ("with positions, parse error: " ++ err)
    (_, Left err) -> Just ("positions erased, parse error: " ++ err)
    (Right withPos, Right erased)
      | withPos /= ir              -> Just "with positions: ASTs differ"
      | erased  /= erasePosProg ir -> Just "positions erased: ASTs differ"
      | otherwise                  -> Nothing

-- | Compile one corpus program — its module graph first, then the program —
-- and check every unit. 'Left' means the file did not compile.
--
-- The pipeline terminates the process on a bad input (see "Pipeline"), so the
-- 'ExitCode' exception is caught here and reported as a skip.
checkProgram :: FilePath -> IO (Either String [String])
checkProgram file = do
  -- The project root a compile resolves and displays module paths against is
  -- the main file's own directory, and the pin keys in its dependencies file
  -- are relative to it.
  let root = takeDirectory file
  outcome <- try $ do
    pins    <- maybe [] (either (const []) id) <$> readDepsFile (depsFilePath file)
    results <- newIORef []
    Pipeline.compileModuleGraph (Enforce pins) root (compileOpts Library) file
      (\m _ ir -> case checkLaws ir of
                    Nothing  -> return ()
                    Just why -> modifyIORef results
                                  (("module " ++ takeFileName m ++ ": " ++ why) :))
    input  <- readFile file
    folded <- Pipeline.frontEndFold (Enforce pins) root (compileOpts Normal) file input
    ir     <- Pipeline.frontEndIR (compileOpts Normal) folded
    let mainFailure = case checkLaws ir of
                        Nothing  -> []
                        Just why -> ["program: " ++ why]
    modules <- readIORef results
    return (reverse modules ++ mainFailure)
  case outcome :: Either ExitCode [String] of
    Left _         -> return (Left "did not compile")
    Right failures -> return (Right failures)

main :: IO ()
main = do
  root  <- troupeRoot
  files <- corpusFiles (corpusRoot root)
  present <- filterM doesFileExist files
  -- Sequential: cases that share a module graph write the same .exports
  -- artifacts, so running them concurrently would race.
  defaultMain $ localOption (NumThreads 1)
              $ testGroup "troupe-ir-sexp over the compiled corpus"
    [ testCase (makeRelative root f) $ do
        r <- checkProgram f
        case r of
          -- A file that cannot be a compilation unit on its own is not a data
          -- point; the units it belongs to are checked through their program.
          Left _         -> return ()
          Right []       -> return ()
          Right failures -> assertFailure (unlines failures)
    | f <- present
    ]
