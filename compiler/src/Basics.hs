{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase #-}

module Basics
where

import GHC.Generics(Generic)
import Data.Serialize (Serialize)

type VarName = String
type FieldName = String

-- | The base32hex-rendered SHA-256 identity of a datatype declaration group
-- (see 'SynVarHash.groupHash').
type GroupHash = String

-- | The canonical ASCII s-expression of a datatype declaration group (see
-- 'SynVarHash.canonicalGroup'), from which its 'GroupHash' is recomputed.
type CanonicalForm = String

-- | Tag on a tuple constructor marking whether the tuple encodes a
-- syntactic-variant value. False for ordinary tuples.
type SynVariantTag = Bool

-- | Eq and Neq: deep equality check on the two parameters, including the types (any type inequality results in false being returned).
data BinOp = Plus | Minus | Mult | Div | Mod |  Eq | Neq | Le | Lt | Ge | Gt | And | Or | RaisedTo | Concat| IntDiv | BinAnd | BinOr | BinXor | BinShiftLeft | BinShiftRight | BinZeroShiftRight | HasField | LatticeJoin
  deriving (Eq,Generic, Ord)
instance Serialize BinOp
data UnaryOp = IsList | IsTuple | IsRecord | Head | Tail | ListLength | TupleLength | RecordSize | LevelOf | UnMinus | Not
  deriving (Eq, Generic, Ord)
instance Serialize UnaryOp

-- | Associativity of a declared operator fixity: @infixl@ / @infixr@ /
-- @infix@ (non-associative, Haskell's meaning — SML's bare @infix@ is left).
data OpAssoc = OpLeft | OpRight | OpNon
  deriving (Eq, Show, Generic, Ord)
instance Serialize OpAssoc

-- | A declared operator fixity: associativity and level (0 through 9).
-- Serializable because it travels with imports in 'ImportDecl'.
data Fixity = Fixity OpAssoc Int
  deriving (Eq, Show, Generic, Ord)
instance Serialize Fixity

instance Show BinOp where
  show Plus  = "+"
  show Minus = "-"
  show Mult  = "*"
  show Div   = "/"
  show IntDiv = "div"
  show Mod   = "mod"
  show Eq    = "="
  show Neq   = "<>"
  show Le    = "<="
  show Lt    = "<"
  show Ge    = ">="
  show Gt    = ">"  
  show And   = "&&"
  show Or    = "||"
  show RaisedTo = "raisedTo"
  show Concat   = "^"
  show BinAnd = "andb" 
  show BinOr =  "orb" 
  show BinXor = "xorb" 
  show BinShiftLeft = "<<"
  show BinShiftRight = ">>" 
  show BinZeroShiftRight = "~>>"
  show HasField = "hasField"
  show LatticeJoin = "join"

instance Show UnaryOp where
  show IsList = "is-list"
  show IsTuple = "is-tuple"
  show Head = "list-head"
  show Tail = "list-tail"
  show ListLength = "list-length"
  show TupleLength = "tuple-length"
  show RecordSize = "record-size"
  show LevelOf = "levelOf"
  show UnMinus = "un-minus"
  show IsRecord = "is-record"
  show Not = "not"


type Precedence = Integer

opPrec :: BinOp -> Precedence

opPrec LatticeJoin = 300

opPrec Mult   = 200
opPrec IntDiv = 200
opPrec Div    = 200
opPrec Mod    = 200

opPrec Plus   = 100
opPrec Minus  = 100
opPrec Concat = 100

opPrec BinShiftLeft      = 70
opPrec BinShiftRight     = 70
opPrec BinZeroShiftRight = 70

opPrec BinAnd = 60
opPrec BinOr  = 60
opPrec BinXor = 60

opPrec Eq    = 50
opPrec Neq   = 50
opPrec Le    = 50
opPrec Lt    = 50
opPrec Ge    = 50
opPrec Gt    = 50
opPrec And   = 50
opPrec Or    = 50
opPrec RaisedTo   = 50
opPrec HasField   = 50

newtype LibName = LibName String deriving (Eq, Show, Generic, Ord)
instance Serialize LibName

data ImportMode = Qualified | Unqualified
  deriving (Eq, Show, Ord, Generic)
instance Serialize ImportMode



-- 2018-07-02; AA: note on the data structure that we use for imports:
-- For each `import` declaration, the parser returns the name of the
-- library that is imported together with a Nothing value. After
-- parsing we produce a version where we replace the Nothing value
-- with the list of names that are exported from the library.
--
-- 2024: Extended to support:
--   - `as` aliases: import List as L
--   - Selective imports: import List (head, tail)

data ImportDecl = ImportDecl
  { importLib      :: LibName          -- Bound name: the library name, or the module's last path segment
  , importPath     :: Maybe String     -- Just the literal path for a module import (import "./..."); Nothing for a library
  , importAlias    :: Maybe LibName    -- Optional alias (from "as X")
  , importExports  :: Maybe [VarName]  -- Value exports from .exports file (filled by ProcessImports)
  , importSelected :: Maybe [VarName]  -- Selective imports (user-specified)
  , importMode     :: ImportMode       -- Qualified | Unqualified
  , importDatatypes :: [(GroupHash, CanonicalForm)]
      -- ^ Datatype groups exported by the library, as (group hash, canonical
      -- form) pairs in declaration order (dependencies precede dependents).
      -- Filled by ProcessImports from the @datatype@ lines of the @.exports@
      -- file; empty until then and for libraries that declare no datatypes.
      -- Datatypes are imported wholesale, independent of 'importSelected'
      -- (they are compile-time only).
  } deriving (Eq, Show, Ord, Generic)

instance Serialize ImportDecl

data Imports = Imports [ImportDecl]
  deriving (Eq, Show, Ord)




op1Prec :: UnaryOp -> Precedence
op1Prec x = 50

appPrec :: Precedence
appPrec = 5000

argPrec :: Precedence
argPrec = appPrec + 1

maxPrec :: Precedence
maxPrec = 100000

consPrec :: Precedence
consPrec = 6000

projPrec :: Precedence 
projPrec = 6100
