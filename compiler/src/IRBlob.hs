{-# LANGUAGE RankNTypes #-}

-- | The serialized IR blob: the framing, the compression, and the two
-- directions across it.
--
-- A blob is the mobile-code format. It is embedded per function in emitted
-- JavaScript (@this.\<fn\>.serialized@) and travels between nodes when a
-- closure moves, opaque to the runtime, which only base64-decodes it and hands
-- it back to the compiler.
--
-- > bytes 0-3 : format identifier "TRPI"
-- > byte  4   : format version, currently 2
-- > bytes 5.. : gzip stream of the UTF-8 encoded troupe-ir-sexp document
--
-- Version 2 replaced version 1's @cereal@ payload, whose encoding was derived
-- from constructor order: adding or reordering a constructor anywhere in the IR
-- silently changed the wire format, and nothing but Haskell could read it. The
-- payload is now the same text format the compiler emits with
-- @--emit-ir-sexp@, so any implementation that can gunzip and read
-- s-expressions can read mobile code. There is no version-1 read path: blobs
-- are our own artifacts, and everything that holds one is rebuilt.
module IRBlob
  ( serializeFunDef
  , encodeBlob
  , deserialize
  , maxDecompressedBytes
  ) where

import           Control.Monad.Except (runExcept)
import qualified Codec.Compression.Zlib.Internal as ZlibI
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Word (Word8)

import           IR (FunDef, SerializationUnit(..), wfFun)
import           IRSexp (parseUnit, printUnit)

irBlobFormatId :: BS.ByteString
irBlobFormatId = BS.pack [0x54, 0x52, 0x50, 0x49]  -- "TRPI"

irBlobVersion :: Word8
irBlobVersion = 2

-- | Upper bound on decompressed blob size, enforced during decompression to
-- defend against decompression bombs arriving from remote nodes. The largest
-- observed real blob is ~10 KB.
maxDecompressedBytes :: Int
maxDecompressedBytes = 64 * 1024 * 1024

encodeBlob :: SerializationUnit -> BS.ByteString
encodeBlob su =
  let text = TE.encodeUtf8 (T.pack (printUnit su))
      gz   = BSL.toStrict (compressGzip text)
  in irBlobFormatId `BS.append` BS.singleton irBlobVersion `BS.append` gz

serializeFunDef :: FunDef -> BS.ByteString
serializeFunDef fdef = encodeBlob (FunSerialization fdef)

compressGzip :: BS.ByteString -> BSL.ByteString
compressGzip =
  ZlibI.compress ZlibI.gzipFormat ZlibI.defaultCompressParams . BSL.fromStrict

-- | Gzip-decompress with a hard output cap, using the incremental zlib API so
-- that corrupt input becomes a Left (rather than an imprecise DecompressError
-- thrown from a lazy thunk) and an over-cap stream is aborted with a Left. The
-- fold accumulator threads the remaining byte budget.
decompressGzipCapped :: Int -> BS.ByteString -> Either String BS.ByteString
decompressGzipCapped cap input =
  BSL.toStrict <$>
    ZlibI.foldDecompressStreamWithInput
      onChunk onEnd onError
      (ZlibI.decompressST ZlibI.gzipFormat ZlibI.defaultDecompressParams)
      (BSL.fromStrict input)
      cap
  where
    onChunk :: BS.ByteString -> (Int -> Either String BSL.ByteString)
                             -> (Int -> Either String BSL.ByteString)
    onChunk c k remaining =
      let n = BS.length c
      in if n > remaining
         then Left ("decompressed IR blob exceeds "
                     ++ show cap ++ "-byte cap")
         else (BSL.fromStrict c <>) <$> k (remaining - n)
    onEnd :: BSL.ByteString -> (Int -> Either String BSL.ByteString)
    onEnd _leftover _ = Right BSL.empty
    onError :: ZlibI.DecompressError -> (Int -> Either String BSL.ByteString)
    onError e _ = Left (show e)

deserialize :: BS.ByteString -> Either String SerializationUnit
deserialize bs
  | not (irBlobFormatId `BS.isPrefixOf` bs) =
      Left "not an IR blob: missing TRPI format identifier"
  | otherwise =
      case BS.uncons (BS.drop 4 bs) of
        Nothing -> Left "truncated IR blob header"
        Just (v, payload)
          | v /= irBlobVersion ->
              Left ("unsupported IR blob format version " ++ show v
                     ++ " (expected " ++ show irBlobVersion ++ "; recompile)")
          | otherwise -> do
              raw  <- decompressGzipCapped maxDecompressedBytes payload
              text <- case TE.decodeUtf8' raw of
                        Left err -> Left ("IR blob payload is not UTF-8: " ++ show err)
                        Right t  -> Right (T.unpack t)
              unit <- parseUnit text
              case unit of
                FunSerialization fdecl
                  | Left _ <- runExcept (wfFun fdecl) -> Left "ir not well-formed"
                _ -> Right unit
