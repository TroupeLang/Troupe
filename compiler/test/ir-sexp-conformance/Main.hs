-- | The troupe-ir-sexp conformance corpus, and the interchange laws over it.
--
-- @ir-sexp-corpus-test@ checks that this compiler agrees with itself over every
-- test program. This suite checks something else: that the format has not moved
-- under reference artifacts recorded earlier, and that a blob produced
-- elsewhere — by Node's zlib rather than Haskell's — decodes to the same IR.
-- Those artifacts are the contract a second implementation is developed and
-- judged against (the Troupe backend, next), which is why they are a
-- conformance corpus rather than test scaffolding.
--
-- Laws, as stated in @compiler/docs/spec-troupe-ir-sexp.md@:
--
--   * L1 round trip:      parse (print x)         == x
--   * L2 text ingestion:  parse (reference .sexp) == this compiler's IR for the program
--   * L3 blob ingestion:  deserialize (reference .blob) == the same value, whoever compressed it
--   * L4 framing:         deserialize (encodeBlob u)  == u
--
-- Equality is structural on the decoded IR throughout. Text layout and
-- compressed bytes are not part of the format: gzip streams from different
-- implementations differ while remaining mutually readable, and the
-- s-expression layer is whitespace-insignificant.
--
-- Regenerate with
-- @IR_SEXP_REGENERATE=1 stack test Troupe-compiler:ir-sexp-conformance-test@.
-- A regenerated document whose text merely differs is an implementation detail;
-- one that no longer parses to the same value is a format change and needs a
-- version bump.
module Main (main) where

import           Control.Monad (forM, unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BSC
import           Data.List (maximumBy)
import           Data.Ord (comparing)
import           System.Directory (createDirectoryIfMissing, doesFileExist, getCurrentDirectory)
import           System.Environment (lookupEnv)
import           System.FilePath (takeDirectory, (</>), (<.>))

import           Test.Tasty
import           Test.Tasty.HUnit

import           CompileMode (CompileMode(..))
import           DepsFile (depsFilePath, readDepsFile)
import           IR (FunDef, IRProgram(..), SerializationUnit(..))
import qualified IRBlob
import           IRSexp (parseProg, parseUnit, printProgWithPos, printUnit)
import qualified Pipeline
import           PrettyPrint (mkPPConfig, parsePosFormat)
import           ProcessImports (PinCheck(..))
import           TroupePositionInfo (unLoc)

-- | The programs the reference artifacts are cut from, chosen to span the grammar:
-- arithmetic and recursion; both float spellings the printer can emit;
-- variant tuples; a structured DC-label literal; lists.
conformancePrograms :: [(String, FilePath)]
conformancePrograms =
  [ ("fib",         "tests/rt/pos/core/fib10.trp")
  , ("floats",      "tests/rt/pos/core/float_literals.trp")
  , ("scientific",  "tests/rt/pos/core/float_scientific.trp")
  , ("variants",    "tests/rt/pos/synvar/option-match.trp")
  , ("dclabel",     "tests/rt/pos/ifc/dclabel-whitespace.trp")
  , ("lists",       "tests/rt/pos/core/list00.trp")
  ]

troupeRoot :: IO FilePath
troupeRoot = takeDirectory <$> getCurrentDirectory

referenceDir :: FilePath -> FilePath
referenceDir root = root </> "compiler" </> "test" </> "ir-sexp-conformance" </> "data"

-- | Compile a reference program to IR with the library pipeline.
compileReference :: FilePath -> FilePath -> IO IRProgram
compileReference root rel = do
  let file = root </> rel
      opts = Pipeline.CompileOpts { Pipeline.coMode     = Normal
                                  , Pipeline.coDump     = Pipeline.silentDump
                                  , Pipeline.coPPConfig = mkPPConfig False (parsePosFormat "inline")
                                  }
  pins   <- maybe [] (either (const []) id) <$> readDepsFile (depsFilePath file)
  input  <- readFile file
  folded <- Pipeline.frontEndFold (Enforce pins) (takeDirectory file) opts file input
  Pipeline.frontEndIR opts folded

-- | The function a reference blob carries: the largest one in the program, by
-- printed size. Taking the first would give the same generated entry wrapper
-- for every program, and every reference blob would exercise the same shapes.
largestFun :: IRProgram -> FunDef
largestFun (IRProgram []) = error "reference program has no functions"
largestFun (IRProgram fs) =
  snd (maximumBy (comparing fst)
        [ (length (printUnit (FunSerialization f)), f) | f <- map unLoc fs ])

main :: IO ()
main = do
  root      <- troupeRoot
  regen     <- lookupEnv "IR_SEXP_REGENERATE"
  let dir = referenceDir root
  createDirectoryIfMissing True dir

  cases <- forM conformancePrograms $ \(name, rel) -> do
    ir <- compileReference root rel
    let sexpPath = dir </> name <.> "sexp"
        blobPath = dir </> name <.> "blob"
        progUnit = ProgramSerialization ir
        funUnit  = FunSerialization (largestFun ir)
    case regen of
      Just _ -> do
        writeFile sexpPath (printProgWithPos ir)
        writeFile blobPath (BSC.unpack (B64.encode (IRBlob.encodeBlob funUnit)) ++ "\n")
      Nothing -> return ()
    return (name, rel, ir, progUnit, funUnit, sexpPath, blobPath)

  defaultMain $ testGroup "troupe-ir-sexp conformance corpus"
    [ testGroup "L1: parse (print x) == x"
        [ testCase name $ progEq (parseProg (printProgWithPos ir)) ir
        | (name, _, ir, _, _, _, _) <- cases ]

    , testGroup "L4: deserialize (encodeBlob u) == u"
        [ testGroup name
            [ testCase "function unit" $
                unitEq (IRBlob.deserialize (IRBlob.encodeBlob funUnit)) funUnit
            , testCase "program unit" $
                unitEq (IRBlob.deserialize (IRBlob.encodeBlob progUnit)) progUnit
            ]
        | (name, _, _, progUnit, funUnit, _, _) <- cases ]

    , testGroup "L2: the checked-in document still parses to this compiler's IR"
        [ testCase name $ do
            exists <- doesFileExist sexpPath
            unless exists $
              assertFailure (sexpPath ++ " is missing; regenerate with IR_SEXP_REGENERATE=1")
            text <- readFile sexpPath
            progEq (parseProg text) ir
        | (name, _, ir, _, _, sexpPath, _) <- cases ]

    , testGroup "L3: the checked-in blob still decodes to this compiler's IR"
        [ testCase name $ do
            exists <- doesFileExist blobPath
            unless exists $
              assertFailure (blobPath ++ " is missing; regenerate with IR_SEXP_REGENERATE=1")
            b64 <- readFile blobPath
            raw <- either (assertFailure . ("base64: " ++)) return
                     (B64.decode (BSC.pack (filter (/= '\n') b64)))
            unitEq (IRBlob.deserialize raw) funUnit
        | (name, _, _, _, funUnit, _, blobPath) <- cases ]

    , testGroup "L3 across implementations: a Node-compressed blob decodes here"
        [ testCase name $ do
            let nodePath = dir </> name <.> "node.blob"
            exists <- doesFileExist nodePath
            unless exists $
              assertFailure (nodePath ++ " is missing; regenerate with\
                             \ node scripts/ir-blob-interchange.mjs")
            b64 <- readFile nodePath
            raw <- either (assertFailure . ("base64: " ++)) return
                     (B64.decode (BSC.pack (filter (/= '\n') b64)))
            unitEq (IRBlob.deserialize raw) funUnit
        | (name, _, _, _, funUnit, _, _) <- cases ]

      -- The other direction of the same law: this blob was written by the Troupe
      -- implementation, from IR it decoded itself, and its payload was compressed
      -- by Node. Nothing about it is this compiler's output.
    , testGroup "L3 across implementations: a Troupe-produced blob decodes here"
        [ testCase name $ do
            let troupePath = dir </> name <.> "troupe.blob"
            exists <- doesFileExist troupePath
            unless exists $
              assertFailure (troupePath ++ " is missing; regenerate with\
                             \ ./scripts/ir-sexp-troupe-conformance.sh --write-troupe-blobs")
            b64 <- readFile troupePath
            raw <- either (assertFailure . ("base64: " ++)) return
                     (B64.decode (BSC.pack (filter (/= '\n') b64)))
            unitEq (IRBlob.deserialize raw) funUnit
        | (name, _, _, _, funUnit, _, _) <- cases ]

    , testGroup "the document a blob carries is the one the printer emits"
        [ testCase name $
            parseUnit (printUnit funUnit) `unitEq'` funUnit
        | (name, _, _, _, funUnit, _, _) <- cases ]
    ]
  where
    -- IR values have no Show instance, so a mismatch is reported by printing
    -- both sides in the format under test.
    progEq got expected =
      case got of
        Left err -> assertFailure ("parse failed: " ++ err)
        Right p  -> assertBool (diff (printProgWithPos p) (printProgWithPos expected))
                               (p == expected)
    unitEq got expected =
      case got of
        Left err -> assertFailure ("decode failed: " ++ err)
        Right u  -> assertBool (diff (printUnit u) (printUnit expected)) (unitSame u expected)
    unitEq' = unitEq
    unitSame (FunSerialization a) (FunSerialization b)         = a == b
    unitSame (ProgramSerialization a) (ProgramSerialization b) = a == b
    unitSame _ _                                               = False
    diff got expected =
      "decoded value differs\n--- decoded ---\n" ++ got
      ++ "\n--- expected ---\n" ++ expected
