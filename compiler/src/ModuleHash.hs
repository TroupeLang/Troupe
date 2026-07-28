-- | Content-addressed identity for program modules.
--
-- A module's identity is the SHA-256 hash of its position-erased codegened IR,
-- rendered as lowercase unpadded base32hex. This mirrors 'SynVarHash' (whose
-- 'base32hexEncode' and hashing idiom are reused): the format identifier
-- 'formatIdentifier' is a hash-input domain separator, not a visible prefix, so
-- the identity string is pure base32hex.
--
-- Hashing the IR is Merkle by the IR's own structure: a dependency reference is
-- a @Lib "module:<dep-hash>" _@ instruction (the dependency's hash is inlined at
-- codegen), so hashing a module's IR necessarily includes its dependencies'
-- hashes. See @_dev_planning/module-system/content-addressed-identity.md@.
module ModuleHash (moduleHash) where

import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.Digest.Pure.SHA       (sha256, bytestringDigest)

import           IR                         (IRProgram)
import qualified IRSexp
import           SynVarHash                 (base32hexEncode)

-- | The format-identifier prefix pinning this identity's IR-encoding version.
-- Version 2 tracks troupe-ir-sexp version 2 (see 'IRSexp.formatVersion'), whose
-- version atom is part of the hashed text.
-- Hashed into the input as a domain separator (never rendered into the
-- identity string), so a future IR-encoding change cannot collide with this
-- version's hashes.
formatIdentifier :: String
formatIdentifier = "troupe:mod:2"

-- | The base32hex-rendered SHA-256 hash of a module's position-erased codegened
-- IR: the module's content-addressed identity. Hashes
-- @formatIdentifier ++ printProg (erasePosProg ir)@ over its ASCII bytes,
-- reusing the canonical position-erased IR s-expression that @--verify-ir-sexp@
-- round-trips.
moduleHash :: IRProgram -> String
moduleHash ir =
  base32hexEncode (BL.unpack (bytestringDigest (sha256 input)))
  where
    input = BLC.pack (formatIdentifier ++ IRSexp.printProg (IRSexp.erasePosProg ir))
