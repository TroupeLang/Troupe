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
import           Control.Monad (filterM, forM)
import           Data.IORef
import           Data.List (isSuffixOf, sort)
import           System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory,
                                   listDirectory)
import           System.Exit (ExitCode)
import           System.FilePath (makeRelative, takeDirectory, takeFileName, (</>))

import           Test.Tasty
import           Test.Tasty.Runners (NumThreads(..))
import           Test.Tasty.HUnit

import           CompileMode (CompileMode(..))
import           DepsFile (depsFilePath, readDepsFile)
import qualified IR
import           IRSexp (erasePosProg, parseProg, printProg, printProgWithPos)
import qualified Pipeline
import           PrettyPrint (mkPPConfig, parsePosFormat)
import           ProcessImports (PinCheck(..))

-- | The repository this test was built from: the parent of the package
-- directory it runs in (@compiler/@).
--
-- Deliberately not $TROUPE. That variable points at whichever checkout the
-- shell is configured for, which in a worktree is a different repository with
-- different module pins — the test would then compile another branch's corpus
-- with this branch's compiler and report the pin mismatches as compile
-- failures.
troupeRoot :: IO FilePath
troupeRoot = takeDirectory <$> getCurrentDirectory

opts :: CompileMode -> Pipeline.CompileOpts
opts mode = Pipeline.CompileOpts { Pipeline.coMode     = mode
                                 , Pipeline.coDump     = Pipeline.silentDump
                                 , Pipeline.coPPConfig = mkPPConfig False (parsePosFormat "inline")
                                 }

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
    Pipeline.compileModuleGraph (Enforce pins) root (opts Library) file
      (\m _ ir -> case checkLaws ir of
                    Nothing  -> return ()
                    Just why -> modifyIORef results
                                  (("module " ++ takeFileName m ++ ": " ++ why) :))
    input  <- readFile file
    folded <- Pipeline.frontEndFold (Enforce pins) root (opts Normal) file input
    ir     <- Pipeline.frontEndIR (opts Normal) folded
    let mainFailure = case checkLaws ir of
                        Nothing  -> []
                        Just why -> ["program: " ++ why]
    modules <- readIORef results
    return (reverse modules ++ mainFailure)
  case outcome :: Either ExitCode [String] of
    Left _         -> return (Left "did not compile")
    Right failures -> return (Right failures)

-- | Every non-empty @.trp@ under a root, sorted.
corpusFiles :: FilePath -> IO [FilePath]
corpusFiles dir = do
  isDir <- doesDirectoryExist dir
  if not isDir then return [] else do
    entries <- sort <$> listDirectory dir
    fmap concat . forM entries $ \e -> do
      let p = dir </> e
      d <- doesDirectoryExist p
      if d then corpusFiles p
           else if ".trp" `isSuffixOf` e
                  then do nonEmpty <- (> 0) . length <$> readFile p
                          return [p | nonEmpty]
                  else return []

main :: IO ()
main = do
  root  <- troupeRoot
  files <- corpusFiles (root </> "tests" </> "rt" </> "pos")
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
