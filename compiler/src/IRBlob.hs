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
      gz   = setGzipOS (BSL.toStrict (compressGzip text))
  in irBlobFormatId `BS.append` BS.singleton irBlobVersion `BS.append` gz

-- | RFC 1952's "unknown" value for the gzip header's OS field, and the value
-- its compliance section names as the default for a compressor that fills
-- nothing in ("255 for OS, 0 for all others").
gzipOSUnknown :: Word8
gzipOSUnknown = 255

-- | Overwrite the gzip header's OS field (offset 9) with 'gzipOSUnknown'.
--
-- zlib writes there whichever platform it was compiled for: 19 from a macOS
-- build, 3 from a Unix one, 10 from Windows. Every other field of the header we
-- produce is already constant -- the modification time is zeroed, and no
-- filename, comment or extra field is set -- so this byte is the only part of a
-- blob that varies with where the compiler was built. Blobs are mobile code and
-- travel to other nodes, which would otherwise learn that much about the sender
-- for free.
--
-- Nothing reads the field. RFC 1952 says a decompressor "may ignore FTEXT and
-- OS and always produce binary output, and still be compliant"; the WHATWG
-- Compression Standard requires DecompressionStream to ignore it; GNU gzip's
-- reader comments "Ignore OS type"; zlib surfaces it only through
-- inflateGetHeader, which we never call. Go, Java, Rust and Python all write
-- 255 for the same reason.
--
-- The zlib binding gives no way to set this at compression time: its
-- CompressParams has no header record, and deflateSetHeader is not bound. The
-- framing around the stream is ours, so it is set here instead.
setGzipOS :: BS.ByteString -> BS.ByteString
setGzipOS gz
  | BS.length gz > 9 = BS.concat [BS.take 9 gz, BS.singleton gzipOSUnknown, BS.drop 10 gz]
  | otherwise        = gz  -- not a well-formed gzip stream; leave it to the reader to reject

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
    -- Input after the gzip member is refused rather than discarded. A blob is
    -- exactly a header and one stream, so trailing bytes mean the blob is not
    -- what its writer produced -- and a reader that ignores them accepts inputs
    -- another implementation rejects, which is a divergence in what the format
    -- admits. (Node's gunzip reads a second member there and fails.)
    onEnd :: BSL.ByteString -> (Int -> Either String BSL.ByteString)
    onEnd leftover _
      | BSL.null leftover = Right BSL.empty
      | otherwise         = Left (show (BSL.length leftover)
                                   ++ " bytes of trailing input after the gzip stream")
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
