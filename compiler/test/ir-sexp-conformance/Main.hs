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
--   * L5 rejection:       every malformed blob is a Left, never a Right and never an exception
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

import qualified Codec.Compression.GZip as GZip
import           Control.Monad (forM, unless)
import           Data.Bits (xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import           Data.List (maximumBy)
import           Data.Ord (comparing)
import           System.Directory (createDirectoryIfMissing, doesFileExist, getCurrentDirectory,
                                  withCurrentDirectory)
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

-- | Every way a blob arriving from another node can be wrong, derived from a
-- valid one. Each must come back as a Left: a reader that accepts nonsense is
-- not conformant, and the second implementation is held to the same list
-- (trp-compiler/conformance.trp).
malformations :: BS.ByteString -> [(String, BS.ByteString)]
malformations good =
  [ ("empty input",                       BS.empty)
  , ("identifier only",                   BS.take 4 good)
  , ("header only",                       BS.take 5 good)
  , ("truncated to a quarter",            BS.take (n `div` 4) good)
  , ("truncated to a half",               BS.take (n `div` 2) good)
  , ("truncated by one byte",             BS.take (n - 1) good)
  , ("a foreign identifier",              BSC.pack "TRPX" <> BS.drop 4 good)
  , ("a version we do not read",          withVersion 3)
  , ("version 1, which carried cereal",   withVersion 1)
  , ("a header with no gzip stream",      header <> BSC.pack "not compressed")
  , ("a byte flipped mid-stream",         flipAt (n `div` 2))
  , ("a byte flipped near the end",       flipAt (n - 3))
  , ("a gzip stream of text that is not a document", header <> gz (BSC.pack "hello"))
  , ("a gzip stream of invalid UTF-8",    header <> gz (BS.pack [0xff, 0xfe, 0xfd]))
  , ("a decompression bomb",              header <> bomb)
  , ("a valid stream with rubbish after it", good <> BSC.pack "rubbish")
  ]
  where
    n           = BS.length good
    header      = BS.take 5 good
    withVersion v = BS.take 4 good <> BS.singleton v <> BS.drop 5 good
    flipAt i    = BS.take i good
                  <> BS.singleton (BS.index good i `xor` 0xff)
                  <> BS.drop (i + 1) good
    gz          = BSL.toStrict . GZip.compress . BSL.fromStrict
    -- 68 MiB of one byte, past the 64 MiB cap, compressed lazily so the
    -- uncompressed form is never held.
    bomb        = BSL.toStrict (GZip.compress (BSL.replicate (68 * 1024 * 1024) 0x61))

troupeRoot :: IO FilePath
troupeRoot = takeDirectory <$> getCurrentDirectory

referenceDir :: FilePath -> FilePath
referenceDir root = root </> "compiler" </> "test" </> "ir-sexp-conformance" </> "data"

-- | Compile a reference program to IR with the library pipeline.
--
-- The compiler records a position under the name it was handed for the
-- compilation unit ('Pipeline.frontEndFold' passes it to the parser), so this
-- names the program by its repository-relative path, from the repository root
-- as the working directory. Every position in the corpus is then relative, and
-- a reference document is the same text in every checkout; compiling from an
-- absolute path would write the regenerating checkout's prefix into the corpus
-- and make it unreadable anywhere else.
compileReference :: FilePath -> IO IRProgram
compileReference rel = do
  let opts = Pipeline.CompileOpts { Pipeline.coMode       = Normal
                                  , Pipeline.coDump       = Pipeline.silentDump
                                  , Pipeline.coPPConfig   = mkPPConfig False (parsePosFormat "inline")
                                  }
  pins   <- maybe [] (either (const []) id) <$> readDepsFile (depsFilePath rel)
  input  <- readFile rel
  folded <- Pipeline.frontEndFold (Enforce pins) (takeDirectory rel) opts rel input
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

  -- Compiled from the repository root so that the paths in 'conformancePrograms'
  -- are the paths the compiler records. 'dir' is already absolute, so the
  -- reference files are written to the same place either way.
  cases <- withCurrentDirectory root $ forM conformancePrograms $ \(name, rel) -> do
    ir <- compileReference rel
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

      -- The other direction of L2: the document was printed by the Troupe
      -- implementation, from IR it decoded itself, so a pass means each side
      -- reads what the other writes -- for whole programs, positions included.
    , testGroup "L2 across implementations: a Troupe-printed document parses here"
        [ testCase name $ do
            let troupePath = dir </> name <.> "troupe.sexp"
            exists <- doesFileExist troupePath
            unless exists $
              assertFailure (troupePath ++ " is missing; regenerate with\
                             \ ./scripts/ir-sexp-troupe-conformance.sh --write-troupe-references")
            text <- readFile troupePath
            progEq (parseProg text) ir
        | (name, _, ir, _, _, _, _) <- cases ]

      -- The other direction of the same law: this blob was written by the Troupe
      -- implementation, from IR it decoded itself, and its payload was compressed
      -- by Node. Nothing about it is this compiler's output.
    , testGroup "L3 across implementations: a Troupe-produced blob decodes here"
        [ testCase name $ do
            let troupePath = dir </> name <.> "troupe.blob"
            exists <- doesFileExist troupePath
            unless exists $
              assertFailure (troupePath ++ " is missing; regenerate with\
                             \ ./scripts/ir-sexp-troupe-conformance.sh --write-troupe-references")
            b64 <- readFile troupePath
            raw <- either (assertFailure . ("base64: " ++)) return
                     (B64.decode (BSC.pack (filter (/= '\n') b64)))
            unitEq (IRBlob.deserialize raw) funUnit
        | (name, _, _, _, funUnit, _, _) <- cases ]

      -- A reader that accepts nonsense is not conformant either. Mobile code
      -- arrives from other nodes, so every one of these is reachable input.
    , testGroup "L5: a malformed blob is refused"
        [ testGroup name
            [ testCase what $
                case IRBlob.deserialize bad of
                  Left _  -> return ()
                  Right _ -> assertFailure "accepted"
            | (what, bad) <- malformations (IRBlob.encodeBlob funUnit) ]
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
