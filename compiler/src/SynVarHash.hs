-- | Declaration hashing for syntactic variants.
--
-- Turns a resolved group of @datatype@ declarations into a canonical ASCII
-- s-expression, hashes it with SHA-256, and renders the digest as lowercase
-- unpadded base32hex. See
-- @_dev_planning/syntactic-variants/normalization.md@ (sections 4, 5, 6) for
-- the normal form, canonical grammar, and worked vectors.
--
-- This module is self-contained: it operates on its own intermediate
-- representation of resolved declaration groups, independent of the parser
-- AST. The desugarer is expected to map into 'Group' once name resolution has
-- produced the de Bruijn indices, same-group references, and external group
-- hashes that the normal form requires.
--
-- The caller supplies resolved content; canonicalization (sorting members by
-- datatype name and constructors by constructor name) happens here.
module SynVarHash
  ( -- * Intermediate representation
    TyNF(..)
  , Constructor
  , Datatype
  , Group
    -- * Canonicalization and hashing
  , canonicalGroup
  , groupHash
  , constructorTag
    -- * Building blocks (exposed for testing)
  , base32hexEncode
  ) where

import           Data.Bits           (shiftL, shiftR, (.&.))
import           Data.List           (sortOn)
import           Data.Word           (Word8)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.Digest.Pure.SHA (sha256, bytestringDigest)

-- | Payload type normal form (spec section 4).
--
-- @Prod@ carries at least two components (an n-ary product with n >= 2); the
-- caller is responsible for not constructing degenerate products.
data TyNF
  = Var Int          -- ^ de Bruijn index of a type variable
  | Prim String      -- ^ primitive token (e.g. @int@, @string@)
  | In String        -- ^ reference to a member of this same group, by name
  | Ext String String -- ^ reference to a previously hashed group: (group hash, member name)
  | Prod [TyNF]      -- ^ n-ary product (n >= 2)
  | App TyNF String  -- ^ application of a built-in / referenced constructor
  deriving (Eq, Show)

-- | A constructor: its name and either its payload normal form ('Just') or
-- nothing for a nullary constructor.
type Constructor = (String, Maybe TyNF)

-- | A datatype: name, number of type parameters, and its constructors.
type Datatype = (String, Int, [Constructor])

-- | A declaration group: the list of mutually recursive datatypes forming one
-- hashing unit.
type Group = [Datatype]

------------------------------------------------------------
-- Canonical encoding (spec section 5)
------------------------------------------------------------

-- | Wrap parts in a single-space-separated parenthesized s-expression node.
sexp :: [String] -> String
sexp parts = "(" ++ unwords parts ++ ")"

-- | Render a payload type normal form.
renderTy :: TyNF -> String
renderTy (Var i)    = sexp ["var", show i]
renderTy (Prim p)   = sexp ["prim", p]
renderTy (In n)     = sexp ["in", n]
renderTy (Ext h n)  = sexp ["ext", h, n]
renderTy (Prod tys) = sexp ("prod" : map renderTy tys)
renderTy (App ty c) = sexp ["app", renderTy ty, c]

-- | Render one constructor (nullary or unary).
renderCtor :: Constructor -> String
renderCtor (cname, Nothing) = sexp ["ctor", cname]
renderCtor (cname, Just ty) = sexp ["ctor", cname, renderTy ty]

-- | Render one datatype: constructors sorted by name.
renderDt :: Datatype -> String
renderDt (name, nparams, ctors) =
  sexp (["dt", name, show nparams] ++ map renderCtor (sortOn fst ctors))

-- | The canonical ASCII s-expression for a group: members sorted by datatype
-- name, constructors sorted by constructor name. Single spaces, no trailing
-- whitespace.
canonicalGroup :: Group -> String
canonicalGroup dts =
  sexp ("group" : map renderDt (sortOn dtName dts))
  where dtName (n, _, _) = n

------------------------------------------------------------
-- Hashing (spec section 5)
------------------------------------------------------------

-- | The format-identifier prefix pinning this normalization/encoding version.
formatIdentifier :: String
formatIdentifier = "troupe:synvar:1"

-- | Lowercase RFC 4648 base32hex alphabet (0-9 a-v).
base32hexAlphabet :: String
base32hexAlphabet = "0123456789abcdefghijklmnopqrstuv"

-- | Encode raw bytes as lowercase unpadded base32hex. Bits are consumed most
-- significant first and the final group is zero-padded on the right (52
-- characters for a 32-byte / 256-bit digest).
base32hexEncode :: [Word8] -> String
base32hexEncode bytes =
  [ base32hexAlphabet !! fromIntegral ((n' `shiftR` (5 * i)) .&. 31)
  | i <- [nchars - 1, nchars - 2 .. 0] ]
  where
    nbits  = length bytes * 8
    nchars = (nbits + 4) `div` 5
    pad    = nchars * 5 - nbits
    n      = foldl (\acc w -> acc * 256 + fromIntegral w) (0 :: Integer) bytes
    n'     = n `shiftL` pad

-- | The base32hex-rendered SHA-256 hash of a group: the group's global
-- identity. Hashes @formatIdentifier ++ canonicalGroup g@ over its ASCII
-- bytes.
groupHash :: Group -> String
groupHash g =
  base32hexEncode (BL.unpack (bytestringDigest (sha256 input)))
  where
    input = BLC.pack (formatIdentifier ++ canonicalGroup g)

-- | The runtime constructor tag: @groupHash ++ "#" ++ datatypeName ++ "#" ++
-- constructorName@. The caller passes the group hash (as returned by
-- 'groupHash') so it is computed once per group.
constructorTag :: String -> String -> String -> String
constructorTag gh dtName ctorName = gh ++ "#" ++ dtName ++ "#" ++ ctorName
