-- | QuickCheck generators of arbitrary 'IRProgram' values for the R1 property
-- test. These deliberately avoid the values the format cannot represent:
--
--   * no NaN / +/-Infinity floats (no literal form; see the spec),
--   * ProjIdx indices in [0, 2^31-1] (the IR.hs bound),
--
-- Programs are generated with 'NoPos' positions throughout; the R1 property
-- erases positions on both sides regardless.
module Gen (genProg) where

import           Test.QuickCheck

import           IR
import qualified Core
import           Basics (BinOp(..), UnaryOp(..), LibName(..))
import           DCLabels
import           RetCPS (VarName(..))
import           TroupePositionInfo (noLoc)

-- | Character set for generated names: ordinary identifier characters plus
-- characters that exercise the string-escaping path (quote, backslash,
-- whitespace, control, and non-ASCII).
nameChars :: [Char]
nameChars =
  ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
  ++ "$._" ++ " \"\\\n\t\r" ++ ['\x00', '\x1f', '\955', '\9731']

genName :: Gen String
genName = do
  k <- choose (0, 6)
  vectorOf k (elements nameChars)

genVarName :: Gen VarName
genVarName = VN <$> genName

genHFN :: Gen HFN
genHFN = HFN <$> genName

genInteger :: Gen Integer
genInteger = do
  base  <- arbitrary
  scale <- elements [1, 1, 1, 2 ^ (64 :: Int), 2 ^ (128 :: Int)]
  return (base * scale)

genDouble :: Gen Double
genDouble = do
  d <- arbitrary
  return (if isNaN d || isInfinite d then 0 else d)

genWord :: Gen Word
genWord = fromIntegral <$> (choose (0, 2147483647) :: Gen Int)

------------------------------------------------------------
-- Variable access, literals, labels.
------------------------------------------------------------

genVA :: Gen VarAccess
genVA = oneof
  [ VarLocal <$> genVarName
  , VarEnv   <$> genVarName
  , pure VarFunSelfRef
  ]

genLVA :: Gen LVarAccess
genLVA = noLoc <$> genVA

genLabelOp :: Gen LabelOp
genLabelOp = elements [Conj, Disj]

genLabelExp :: Int -> Gen LabelExp
genLabelExp n
  | n <= 0    = TagExp <$> genName
  | otherwise = oneof
      [ TagExp <$> genName
      , OpExp <$> genLabelOp <*> genLabelExp (n `div` 2) <*> genLabelExp (n `div` 2)
      ]

genComponent :: Int -> Gen LabelComponent
genComponent n = oneof
  [ pure (ConstComponent LabelTrue)
  , pure (ConstComponent LabelFalse)
  , ExprComponent <$> genLabelExp n
  ]

genDCLabel :: Gen DCLabelExp
genDCLabel = do
  n <- choose (0, 3)
  c <- genComponent n
  i <- genComponent n
  return (DCLabelExp (c, i))

genLit :: Gen Core.Lit
genLit = oneof
  [ Core.LNumeric . Core.NumInt   <$> genInteger
  , Core.LNumeric . Core.NumFloat <$> genDouble
  , Core.LString  <$> genName
  , Core.LBool    <$> arbitrary
  , pure Core.LUnit
  , Core.LAtom    <$> genName
  , Core.LLabel   <$> genName
  , Core.LDCLabel <$> genDCLabel
  ]

------------------------------------------------------------
-- Expressions (IRExpr fields are all VarAccess/lits — non-recursive).
------------------------------------------------------------

genBinOp :: Gen BinOp
genBinOp = elements
  [ Plus, Minus, Mult, Div, Mod, Eq, Neq, Le, Lt, Ge, Gt, And, Or
  , RaisedTo, Concat, IntDiv, BinAnd, BinOr, BinXor
  , BinShiftLeft, BinShiftRight, BinZeroShiftRight, HasField
  , LatticeJoin ]

genUnOp :: Gen UnaryOp
genUnOp = elements
  [ IsList, IsTuple, IsRecord, Head, Tail, ListLength
  , TupleLength, RecordSize, LevelOf, UnMinus, Not ]

genField :: Gen (String, LVarAccess)
genField = (,) <$> genName <*> genLVA

genSmallList :: Gen a -> Gen [a]
genSmallList g = do
  k <- choose (0, 3)
  vectorOf k g

genExpr :: Gen IRExpr
genExpr = oneof
  [ Bin <$> genBinOp <*> genLVA <*> genLVA
  , Un  <$> genUnOp  <*> genLVA
  , Tuple      <$> genSmallList genLVA <*> elements [False, True]
  , Record     <$> genSmallList genField
  , WithRecord <$> genLVA <*> genSmallList genField
  , ProjField  <$> genLVA <*> genName
  , ProjIdx    <$> genLVA <*> genWord
  , List       <$> genSmallList genLVA
  , ListCons   <$> genLVA <*> genLVA
  , Const <$> genLit
  , Base  <$> genName
  , Lib   <$> (LibName <$> genName) <*> genName
  ]

genInst :: Gen IRInst
genInst = oneof
  [ Assign <$> genVarName <*> genExpr
  , MkFunClosures
      <$> genSmallList ((,) <$> genVarName <*> genLVA)
      <*> genSmallList ((,) <$> genVarName <*> genHFN)
  ]

------------------------------------------------------------
-- Basic blocks / terminators (recursion bounded by a depth parameter).
------------------------------------------------------------

genBB :: Int -> Gen IRBBTree
genBB depth = do
  insts <- genSmallList (noLoc <$> genInst)
  term  <- genTerm depth
  return (BB insts (noLoc term))

genTerm :: Int -> Gen IRTerminator
genTerm depth
  | depth <= 0 = oneof leaves
  | otherwise  = oneof (leaves ++ recs)
  where
    leaves =
      [ TailCall  <$> genLVA <*> genLVA
      , Ret       <$> genLVA
      , LibExport <$> genLVA
      , Error     <$> genLVA
      ]
    recs =
      [ If <$> genLVA <*> sub <*> sub
      , AssertElseError <$> genLVA <*> sub <*> genLVA
      , StackExpand <$> genVarName <*> sub <*> sub
      ]
    sub = genBB (depth - 1)

genFun :: Gen FunDef
genFun = do
  name   <- genName
  arg    <- genName
  consts <- genSmallList ((,) <$> genVarName <*> genLit)
  depth  <- choose (0, 3)
  body   <- genBB depth
  return (FunDef (HFN name) (noLoc (VN arg)) consts body)

genProg :: Gen IRProgram
genProg = do
  atoms <- Core.Atoms <$> genSmallList genName
  funs  <- do k <- choose (1, 3); vectorOf k (noLoc <$> genFun)
  return (IRProgram atoms funs)
