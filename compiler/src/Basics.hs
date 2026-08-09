{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase #-}

module Basics
where

import GHC.Generics(Generic)
import Sexp

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
data UnaryOp = IsList | IsTuple | IsRecord | Head | Tail | ListLength | TupleLength | RecordSize | LevelOf | UnMinus | Not
  deriving (Eq, Generic, Ord)

-- | Associativity of a declared operator fixity: @infixl@ / @infixr@ /
-- @infix@ (non-associative, Haskell's meaning — SML's bare @infix@ is left).
data OpAssoc = OpLeft | OpRight | OpNon
  deriving (Eq, Show, Generic, Ord)

-- | A declared operator fixity: associativity and level (0 through 9).
-- Serializable because it travels with imports in 'ImportDecl'.
data Fixity = Fixity OpAssoc Int
  deriving (Eq, Show, Generic, Ord)

-- | Whether a name is an operator name: all operator characters ('$'-only
-- operators included), or containing a character that is not legal in a
-- JavaScript identifier. Compiler-internal '$'-prefixed names mix '$' with
-- alphanumerics and are not operator names.
isOperatorName :: VarName -> Bool
isOperatorName x =
  not (null x) && (all (`elem` fullOpChars) x || any (`elem` jsHostileOpChars) x)
  where fullOpChars = "!$%&*+-/:<=>?@^|~." :: String

-- | The operator characters that are not legal JavaScript identifier
-- characters ('$' and '.' excluded: '$' is legal, '.' never leads a name).
jsHostileOpChars :: String
jsHostileOpChars = "!%&*+-/:<=>?@^|~."

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

data ImportMode = Qualified | Unqualified
  deriving (Eq, Show, Ord, Generic)

-- | What kind of provider an import declaration names: a stdlib library
-- (@import List@), a program module (@import "./util/Fmt"@), or a native
-- module (@require native SimpleFiles@). 'FromModule' carries the literal
-- path until 'ProcessImports' resolves it, the module's content hash after.
data ImportSource
  = FromLibrary
  | FromModule String
  | FromNative
  deriving (Eq, Show, Ord, Generic)



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
  { importLib      :: LibName          -- Bound name: the library name, the module's last path segment, or the native module name
  , importSource   :: ImportSource     -- Library, module (with its path, later its hash), or native module
  , importAlias    :: Maybe LibName    -- Optional alias (from "as X")
  , importExports  :: Maybe [VarName]  -- Value exports from .exports file (filled by ProcessImports)
  , importSelected :: Maybe [VarName]  -- Selective imports (user-specified)
  , importMode     :: ImportMode       -- Qualified | Unqualified
  , importDatatypes :: [(GroupHash, CanonicalForm)]
      -- ^ Datatype groups exported by the library, as (group hash, canonical
      -- form) pairs in declaration order (dependencies precede dependents).
      -- Filled by ProcessImports from the @datatype@ lines of the @.exports@
      -- file; empty until then and for libraries that declare no datatypes.
  , importFixities :: [(VarName, Fixity)]
      -- ^ Fixities of the exported operators, from the @fixity@ lines of the
      -- @.exports@ file, restricted to the selection when one is given.
      -- Filled by ProcessImports; empty until then. Consumed by the
      -- re-association pass for unqualified imports only (qualified access
      -- is prefix-only and needs no fixity).
      -- Datatypes are imported wholesale, independent of 'importSelected'
      -- (they are compile-time only).
  } deriving (Eq, Show, Ord, Generic)


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


------------------------------------------------------------
-- s-expression serialization (see "Sexp")
------------------------------------------------------------

-- | Operator name tables: Haskell constructor names, verbatim. One table per
-- operator type serves both directions, so the two cannot drift apart. A new
-- operator does not break the build here -- 'nameOf' fails at run time instead;
-- the round-trip suite covers every constructor
-- (@compiler/test/ir-sexp-test@).
binOpTable :: [(String, BinOp)]
binOpTable =
  [ ("Plus", Plus), ("Minus", Minus), ("Mult", Mult), ("Div", Div)
  , ("Mod", Mod), ("Eq", Eq), ("Neq", Neq), ("Le", Le), ("Lt", Lt)
  , ("Ge", Ge), ("Gt", Gt), ("And", And), ("Or", Or)
  , ("RaisedTo", RaisedTo), ("Concat", Concat)
  , ("IntDiv", IntDiv), ("BinAnd", BinAnd), ("BinOr", BinOr)
  , ("BinXor", BinXor), ("BinShiftLeft", BinShiftLeft)
  , ("BinShiftRight", BinShiftRight), ("BinZeroShiftRight", BinZeroShiftRight)
  , ("HasField", HasField), ("LatticeJoin", LatticeJoin)
  ]

unOpTable :: [(String, UnaryOp)]
unOpTable =
  [ ("IsList", IsList), ("IsTuple", IsTuple), ("IsRecord", IsRecord)
  , ("Head", Head), ("Tail", Tail)
  , ("ListLength", ListLength), ("TupleLength", TupleLength)
  , ("RecordSize", RecordSize), ("LevelOf", LevelOf)
  , ("UnMinus", UnMinus), ("Not", Not)
  ]

nameOf :: Eq a => String -> [(String, a)] -> a -> String
nameOf what tbl x =
  case [ n | (n, y) <- tbl, y == x ] of
    (n : _) -> n
    []      -> error ("Basics.nameOf: missing " ++ what)

instance Sexp BinOp where
  toSexp = Atom . nameOf "BinOp" binOpTable
  fromSexp d = do
    s <- asToken d
    case lookup s binOpTable of
      Just op -> Right op
      Nothing -> Left ("unknown binary operator: " ++ s)

instance Sexp UnaryOp where
  toSexp = Atom . nameOf "UnaryOp" unOpTable
  fromSexp d = do
    s <- asToken d
    case lookup s unOpTable of
      Just op -> Right op
      Nothing -> Left ("unknown unary operator: " ++ s)

instance Sexp LibName where
  toSexp (LibName l) = Str l
  fromSexp d = LibName <$> asName d
