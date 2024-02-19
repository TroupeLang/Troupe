{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase #-}

module Basics
where

import GHC.Generics(Generic)
import Data.Serialize (Serialize)

type VarName = String
type AtomName = String
type FieldName = String

-- | Eq and Neq: deep equality check on the two parameters, including the types (any type inequality results in false being returned).
data BinOp = Plus | Minus | Mult | Div | Mod |  Eq | Neq | Le | Lt | Ge | Gt | And | Or | RaisedTo | FlowsTo | Concat| IntDiv | BinAnd | BinOr | BinXor | BinShiftLeft | BinShiftRight | BinZeroShiftRight | HasField | LatticeJoin | LatticeMeet
  deriving (Eq,Generic, Ord)
instance Serialize BinOp
data UnaryOp = IsList | IsTuple | IsRecord | Head | Tail | Fst | Snd | ListLength | TupleLength | LevelOf | UnMinus
  deriving (Eq, Generic, Ord)
instance Serialize UnaryOp

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
  show FlowsTo  = "flowsTo"
  show Concat   = "^"
  show BinAnd = "andb" 
  show BinOr =  "orb" 
  show BinXor = "xorb" 
  show BinShiftLeft = "<<"
  show BinShiftRight = ">>" 
  show BinZeroShiftRight = "~>>"
  show HasField = "hasField"
  show LatticeJoin = "join"
  show LatticeMeet = "meet"

instance Show UnaryOp where
  show IsList = "is-list"
  show IsTuple = "is-tuple"
  show Head = "list-head"
  show Tail = "list-tail"
  show Fst = "fst"
  show Snd = "snd"
  show ListLength = "list-length"
  show TupleLength = "tuple-length"
  show LevelOf = "levelOf"
  show UnMinus = "un-minus"
  show IsRecord = "is-record"


type Precedence = Integer

opPrec :: BinOp -> Precedence

opPrec LatticeJoin = 300
opPrec LatticeMeet = 300

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
opPrec FlowsTo    = 50
opPrec RaisedTo   = 50
opPrec HasField   = 50

newtype LibName = LibName String deriving (Eq, Show, Generic, Ord)
instance Serialize LibName



-- 2018-07-02; AA: note on the data structure that we use for imports:
-- For each `import` declaration, the parser returns the name of the
-- library that is imported together with a Nothing value. After
-- parsing we produce a version where we replace the Nothing value
-- with the list of names that are exported from the library.


data Imports = Imports [(LibName, Maybe [VarName])]
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
