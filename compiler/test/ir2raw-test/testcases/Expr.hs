{-# LANGUAGE StandaloneDeriving #-}

module Expr where

import Data.Functor
import Control.Arrow

import Util
import RetCPS (VarName(..))
import IR
import qualified Core
import TroupePositionInfo
import Basics


mkP :: IRExpr -> IRProgram
mkP e = IRProgram (Core.Atoms []) [FunDef (HFN "main") (VN "arg") [] body]
  where body = BB [Assign (VN "r") e] (LibExport (mkV "r")) -- need to use assigned variable so that it is not optimized away

tcs :: [(String, IRProgram)]
tcs = map (second mkP) $
  (implBinops <&> \op -> (show op, Bin op (mkV "x") (mkV "y"))) ++
  (implUnops <&> \op -> (show op, Un op (mkV "x"))) ++
  [ ("Const", Const (Core.LString "testlit"))
  , ("Base (authorityarg)", Base "$$authorityarg")
  , ("Base (general)", Base "somevar")
  , ("Tuple0", Tuple [])
  , ("Tuple1", Tuple [mkV "v"])
  , ("Tuple2", Tuple [mkV "v1", mkV "v2"])
  , ("List0", List [])
  , ("List1", List [mkV "v"])
  , ("List2", List [mkV "v1", mkV "v2"])
  , ("Record0", Record [])
  , ("Record1", Record [("field1", mkV "v1")])
  , ("Record2", Record [("field1", mkV "v1"), ("field2", mkV "v2")])
  , ("ListCons", ListCons (mkV "x") (mkV "xs"))
  , ("WithRecord0", WithRecord (mkV "x") [])
  , ("WithRecord1", WithRecord (mkV "x") [("field1", mkV "v1")])
  , ("WithRecord2", WithRecord (mkV "x") [("field1", mkV "v1"), ("field2", mkV "v2")])
  , ("ProjField", ProjField (mkV "x") "field1")
  , ("ProjIdx", ProjIdx (mkV "x") 123)
  , ("Lib", Lib (LibName "string") "charAt")
  ]

deriving instance Enum BinOp
deriving instance Bounded BinOp
deriving instance Enum UnaryOp
deriving instance Bounded UnaryOp

binops :: [BinOp]
binops = enumFrom minBound

unops :: [UnaryOp]
unops = enumFrom minBound

-- TODO remove when implemented
notimplBinops :: [BinOp]
notimplBinops =  [And, Or, BinAnd, BinOr, BinXor, BinShiftLeft, BinShiftRight, BinZeroShiftRight, FlowsTo, LatticeJoin, LatticeMeet]

notimplUnops :: [UnaryOp]
notimplUnops = [Fst, Snd, LevelOf]

implBinops = filter (`notElem` notimplBinops) binops

implUnops = filter (`notElem` notimplUnops) unops
