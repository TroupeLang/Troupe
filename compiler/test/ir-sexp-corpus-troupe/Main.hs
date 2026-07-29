-- | The interchange, across implementations, over every program in the corpus.
--
-- @ir-sexp-corpus-test@ checks that this compiler reads back what it prints,
-- for every program in @tests/rt/pos@. This suite checks that the /other/
-- implementation does: each program's document is handed to the Troupe reader
-- in @trp-compiler/@, and what it prints back is parsed here and compared with
-- the IR this compiler started from.
--
--   compile -> printProgWithPos -> Troupe decode -> Troupe encode -> parseProg -> ==
--
-- The conformance corpus does this for six hand-picked programs. This does it
-- for all of them, which is where the shapes nobody thought to pick live.
--
-- One Troupe process handles the whole corpus: process startup is a fixed cost
-- and there are hundreds of documents.
--
-- Not run unless @IR_SEXP_TROUPE_CORPUS@ is set, because it needs a built
-- runtime and built libraries, which @stack test@ alone does not provide.
-- @make test/ir-sexp-corpus-troupe@ builds them and sets it. When it is unset
-- the suite says so rather than passing quietly.
module Main (main) where

import           Control.Monad (filterM, forM, unless)
import           Data.List (isInfixOf)
import           Data.Maybe (catMaybes)
import           System.Directory (createDirectoryIfMissing, doesFileExist, removePathForcibly)
import           System.Environment (lookupEnv)
import           System.Exit (ExitCode(..))
import           System.FilePath (makeRelative, (</>), (<.>))
import           System.Process (readProcessWithExitCode)

import           Test.Tasty
import           Test.Tasty.HUnit

import           Corpus (compileProgram, corpusFiles, corpusRoot, troupeRoot)
import           IRSexp (parseProg, printProgWithPos)

-- | Run a command and return its output, failing the whole suite if it does
-- not succeed: a broken exchange must not look like an empty corpus.
run :: String -> [String] -> IO String
run cmd args = do
  (code, out, err) <- readProcessWithExitCode cmd args ""
  case code of
    ExitSuccess -> return out
    _ -> ioError (userError (unwords (cmd : args) ++ " failed:\n" ++ out ++ err))

main :: IO ()
main = do
  root    <- troupeRoot
  asked   <- lookupEnv "IR_SEXP_TROUPE_CORPUS"
  case asked of
    Nothing ->
      -- Named so that a run which checked nothing cannot be mistaken for one
      -- that did.
      defaultMain $ testCase "not run (needs a built runtime; make test/ir-sexp-corpus-troupe)" $
        return ()
    Just _ -> do
      built <- doesFileExist (root </> "rt" </> "built" </> "troupe.mjs")
      unless built $ ioError (userError "rt/built/troupe.mjs is missing; run 'make rt'")
      files   <- corpusFiles (corpusRoot root) >>= filterM doesFileExist
      -- Sequential: programs sharing a module graph write the same .exports.
      compiled <- fmap catMaybes . forM files $ \f -> do
        ir <- compileProgram f
        return ((,) (makeRelative root f) <$> ir)

      let scratch = root </> "out" </> "ir-sexp-corpus-troupe"
      removePathForcibly scratch
      createDirectoryIfMissing True scratch

      -- Documents are named by index: corpus paths hold separators and the
      -- exchange is by position, not by name.
      let named = zip [(0 :: Int) ..] compiled
      mapM_ (\(i, (_, ir)) -> writeFile (scratch </> show i <.> "sexp") (printProgWithPos ir))
            named

      _ <- run (root </> "bin" </> "troupec")
             [ root </> "trp-compiler" </> "reprint.trp"
             , "-m", "--output=" ++ scratch </> "reprint.js" ]
      out <- run "node"
               [ root </> "rt" </> "built" </> "troupe.mjs"
               , "-f=" ++ scratch </> "reprint.js"
               , "--localonly", "--suppress-local-info-message"
               , "--suppress-main-thread-finished-message"
               , "--io-root=" ++ scratch, "--", "." ]

      -- A thread error leaves the runtime exiting 0, so the exit code alone
      -- would accept a run that read nothing. The reprinter says what it did,
      -- and that is what is believed.
      let expected = "reprinted " ++ show (length named) ++ " of "
                     ++ show (length named) ++ " documents"
      unless (expected `isInfixOf` out) $
        ioError (userError ("the Troupe reader did not report '" ++ expected
                            ++ "'; it said:\n" ++ out))

      defaultMain $ testGroup "troupe-ir-sexp across implementations, over the compiled corpus"
        [ testCase name $ do
            let reprinted = scratch </> show i <.> "reprint"
            exists <- doesFileExist reprinted
            unless exists $ assertFailure "the Troupe reader produced no document for this program"
            text <- readFile reprinted
            case parseProg text of
              Left err -> assertFailure ("parsing what Troupe printed failed: " ++ err)
              Right p  -> assertBool (mismatch (printProgWithPos p) (printProgWithPos ir))
                                     (p == ir)
        | (i, (name, ir)) <- named
        ]
  where
    mismatch got expected =
      "the value differs from the one this compiler started with\n--- via Troupe ---\n" ++ got
      ++ "\n--- expected ---\n" ++ expected
