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
  , TyRef(..)
  , Constructor
  , Datatype
  , Group
    -- * Canonicalization and hashing
  , canonicalGroup
  , groupHash
  , constructorTag
    -- * Parsing (inverse of 'canonicalGroup')
  , parseGroup
    -- * Building blocks (exposed for testing)
  , base32hexEncode
  ) where

import           Data.Bits           (shiftL, shiftR, (.&.))
import           Data.List           (nub, sortOn)
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
  | App [TyNF] TyRef -- ^ n-ary application to a built-in / referenced target
  | Rec [(String, TyNF)]
      -- ^ a record type with n >= 0 fields. Canonicalization sorts the fields
      --   by label, so field order in the source does not affect identity; the
      --   caller is responsible for rejecting duplicate labels.
  deriving (Eq, Show)

-- | The target of an application (spec section 5): a built-in type
-- constructor, a same-group datatype, or a datatype in a previously hashed
-- group.
data TyRef
  = RBuiltin String   -- ^ built-in type constructor (e.g. @list@)
  | RIn String        -- ^ same-group datatype, by name
  | RExt String String -- ^ previously hashed group: (group hash, member name)
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

renderTy :: TyNF -> String
renderTy (Var i)    = sexp ["var", show i]
renderTy (Prim p)   = sexp ["prim", p]
renderTy (In n)     = sexp ["in", n]
renderTy (Ext h n)  = sexp ["ext", h, n]
renderTy (Prod tys) = sexp ("prod" : map renderTy tys)
renderTy (App tys tgt) = sexp ("app" : map renderTy tys ++ [renderTarget tgt])
renderTy (Rec flds) = sexp ("rec" : map renderFld (sortOn fst flds))
  where renderFld (l, t) = sexp ["fld", l, renderTy t]

renderTarget :: TyRef -> String
renderTarget (RBuiltin n) = sexp ["builtin", n]
renderTarget (RIn n)      = sexp ["in", n]
renderTarget (RExt h n)   = sexp ["ext", h, n]

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
-- Parsing (inverse of the canonical encoding, spec section 5)
------------------------------------------------------------

-- | A raw s-expression: an atom or a parenthesized node. Intermediate
-- representation for parsing the canonical form.
data Sx = SAtom String | SNode [Sx]

-- | Tokenize the canonical ASCII s-expression. Parentheses are single-character
-- tokens; every other token is a maximal run of non-space, non-parenthesis
-- characters (identifiers, hashes, and integers, which by construction contain
-- no spaces or parentheses). Single spaces separate tokens and are discarded.
tokenize :: String -> [String]
tokenize [] = []
tokenize (c:cs)
  | c == '('  = "(" : tokenize cs
  | c == ')'  = ")" : tokenize cs
  | c == ' '  = tokenize cs
  | otherwise = let (a, rest) = span (\x -> x /= '(' && x /= ')' && x /= ' ') (c:cs)
                in a : tokenize rest

parseSx :: [String] -> Either String (Sx, [String])
parseSx ("(":ts) = do (kids, ts') <- parseNodes ts
                      return (SNode kids, ts')
parseSx (")":_)  = Left "unexpected ')'"
parseSx (t:ts)   = Right (SAtom t, ts)
parseSx []       = Left "unexpected end of input"

parseNodes :: [String] -> Either String ([Sx], [String])
parseNodes (")":ts) = Right ([], ts)
parseNodes []       = Left "unterminated '('"
parseNodes ts       = do (x, ts')   <- parseSx ts
                         (xs, ts'') <- parseNodes ts'
                         return (x:xs, ts'')

-- | Parse a canonical group string back into a 'Group'. Inverse of
-- 'canonicalGroup': @parseGroup (canonicalGroup g) == Right g'@ where @g'@ is
-- @g@ with members and constructors in canonical (sorted) order. The interface
-- reader recomputes the hash from the parsed form, so this parser is not asked
-- to recover any hash from the surrounding @datatype@ line.
parseGroup :: String -> Either String Group
parseGroup s = do
  (sx, rest) <- parseSx (tokenize s)
  case rest of
    [] -> sxGroup sx
    _  -> Left "trailing tokens after group"

sxGroup :: Sx -> Either String Group
sxGroup (SNode (SAtom "group" : dts)) = mapM sxDt dts
sxGroup _ = Left "expected a (group ...) node"

sxDt :: Sx -> Either String Datatype
sxDt (SNode (SAtom "dt" : SAtom name : SAtom np : ctors)) = do
  n  <- readInt np
  cs <- mapM sxCtor ctors
  return (name, n, cs)
sxDt _ = Left "expected a (dt ...) node"

sxCtor :: Sx -> Either String Constructor
sxCtor (SNode [SAtom "ctor", SAtom name])     = Right (name, Nothing)
sxCtor (SNode [SAtom "ctor", SAtom name, ty]) = do t <- sxTy ty; return (name, Just t)
sxCtor _ = Left "expected a (ctor ...) node"

sxTy :: Sx -> Either String TyNF
sxTy (SNode [SAtom "var", SAtom i])          = Var <$> readInt i
sxTy (SNode [SAtom "prim", SAtom p])         = Right (Prim p)
sxTy (SNode [SAtom "in", SAtom n])           = Right (In n)
sxTy (SNode [SAtom "ext", SAtom h, SAtom n]) = Right (Ext h n)
sxTy (SNode (SAtom "prod" : tys))
  | length tys >= 2                          = Prod <$> mapM sxTy tys
sxTy (SNode (SAtom "app" : rest))
  | length rest >= 2                         = do
      as  <- mapM sxTy (init rest)
      tgt <- sxTarget (last rest)
      return (App as tgt)
sxTy (SNode (SAtom "rec" : flds))            = do
  fs <- mapM sxFld flds
  let ls = map fst fs
  if length (nub ls) /= length ls
    then Left "duplicate field label in a record type node"
    else Right (Rec fs)
sxTy _ = Left "malformed type node"

sxFld :: Sx -> Either String (String, TyNF)
sxFld (SNode [SAtom "fld", SAtom l, ty]) = do t <- sxTy ty; return (l, t)
sxFld _ = Left "expected a (fld ...) node"

sxTarget :: Sx -> Either String TyRef
sxTarget (SNode [SAtom "builtin", SAtom n])     = Right (RBuiltin n)
sxTarget (SNode [SAtom "in", SAtom n])          = Right (RIn n)
sxTarget (SNode [SAtom "ext", SAtom h, SAtom n]) = Right (RExt h n)
sxTarget _ = Left "malformed application target node"

readInt :: String -> Either String Int
readInt str = case reads str of
  [(n, "")] -> Right n
  _         -> Left ("expected an integer, got " ++ show str)

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
