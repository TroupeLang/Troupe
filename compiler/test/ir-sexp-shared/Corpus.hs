-- | What the corpus-wide sexp suites share: where the repository is, which
-- files are corpus programs, and how to compile one.
--
-- Two suites walk @tests/rt/pos@ — @ir-sexp-corpus-test@ checks this compiler
-- against itself, @ir-sexp-corpus-troupe-test@ checks it against the Troupe
-- implementation — and they must walk the same set with the same pins, or a
-- disagreement between them says nothing.
module Corpus
  ( troupeRoot
  , corpusRoot
  , corpusFiles
  , compileOpts
  , compileProgram
  ) where

import           Control.Exception (try)
import           Control.Monad (forM)
import           Data.List (isSuffixOf, sort)
import           System.Directory (doesDirectoryExist, getCurrentDirectory, listDirectory)
import           System.Exit (ExitCode)
import           System.FilePath (takeDirectory, (</>))

import           CompileMode (CompileMode(..))
import           DepsFile (depsFilePath, readDepsFile)
import qualified IR
import qualified Pipeline
import           PrettyPrint (mkPPConfig, parsePosFormat)
import           ProcessImports (PinCheck(..))
import           StdioModel (defaultStdioModel)

-- | The repository a suite was built from: the parent of the package directory
-- it runs in (@compiler/@).
--
-- Deliberately not $TROUPE. That variable points at whichever checkout the
-- shell is configured for, which in a worktree is a different repository with
-- different module pins — the suite would then compile another branch's corpus
-- with this branch's compiler and report the pin mismatches as compile
-- failures.
troupeRoot :: IO FilePath
troupeRoot = takeDirectory <$> getCurrentDirectory

-- | The programs both suites walk.
corpusRoot :: FilePath -> FilePath
corpusRoot root = root </> "tests" </> "rt" </> "pos"

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

compileOpts :: CompileMode -> Pipeline.CompileOpts
compileOpts mode =
  Pipeline.CompileOpts { Pipeline.coMode       = mode
                       , Pipeline.coDump       = Pipeline.silentDump
                       , Pipeline.coPPConfig   = mkPPConfig False (parsePosFormat "inline")
                       , Pipeline.coStdioModel = defaultStdioModel
                       }

-- | Compile one program to IR. 'Nothing' means the file is not a compilation
-- unit on its own: the corpus holds module components whose pins live with the
-- program that imports them (see @docs/MODULES.md@), and compiling one alone
-- stops in import processing before any IR exists, which says nothing about the
-- format. The pipeline terminates the process on bad input, so the 'ExitCode'
-- exception is what that looks like here.
compileProgram :: FilePath -> IO (Maybe IR.IRProgram)
compileProgram file = do
  let root = takeDirectory file
  outcome <- try $ do
    pins   <- maybe [] (either (const []) id) <$> readDepsFile (depsFilePath file)
    input  <- readFile file
    folded <- Pipeline.frontEndFold (Enforce pins) root (compileOpts Normal) file input
    Pipeline.frontEndIR (compileOpts Normal) folded
  case outcome :: Either ExitCode IR.IRProgram of
    Left _   -> return Nothing
    Right ir -> return (Just ir)
