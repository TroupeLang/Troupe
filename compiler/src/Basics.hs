{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}

module Basics
where

import GHC.Generics(Generic)
import Data.Serialize (Serialize)

type VarName = String
type AtomName = String

data BinOp = Plus | Minus | Mult | Div | Mod | Eq | Neq | Le | Lt | Ge | Gt | And | Or | Index | RaisedTo | FlowsTo | Concat 
  deriving (Eq,Generic)
instance Serialize BinOp
data UnaryOp = IsList | IsTuple | Head | Tail | Fst | Snd | Length | LevelOf | UnMinus
  deriving (Eq, Generic)
instance Serialize UnaryOp

instance Show BinOp where
  show Plus  = "+"
  show Minus = "-"
  show Mult  = "*"
  show Div   = "/"
  show Mod   = "%"
  show Eq    = "="
  show Neq   = "<>"
  show Le    = "<="
  show Lt    = "<"
  show Ge    = ">="
  show Gt    = ">"  
  show And   = "&&"
  show Index = "!!"
  show RaisedTo = "raisedTo"
  show FlowsTo  = "flowsTo"
  show Concat   = "^"



instance Show UnaryOp where
  show IsList = "is-list"
  show IsTuple = "is-tuple"
  show Head = "tail"
  show Tail = "head"
  show Fst = "fst"
  show Snd = "snd"
  show Length = "length"
  show LevelOf = "levelOf"

{--

evalBinOp :: Integral a => BinOp -> a -> a -> a
evalBinOp op a b =
   case op of
      Plus  -> a + b
      Minus -> a - b
      Mult  -> a * b
      Div   -> a `div` b
      Eq    -> boolToInt $ a == b
      Neq   -> boolToInt $ a /= b
      Gt    -> boolToInt $ a >  b
      Ge    -> boolToInt $ a >= b
      Lt    -> boolToInt $ a < b
      Le    -> boolToInt $ a <= b
    where boolToInt True  = 1
          boolToInt False = 0

--}

newtype LibName = LibName String deriving (Eq, Show, Generic)
instance Serialize LibName



-- 2018-07-02; AA: note on the data structure that we use for imports:
-- For each `import` declaration, the parser returns the name of the
-- library that is imported together with a Nothing value. After
-- parsing we produce a version where we replace the Nothing value
-- with the list of names that are exported from the library.


data Imports = Imports [(LibName, Maybe [VarName])]
  deriving (Eq, Show)

