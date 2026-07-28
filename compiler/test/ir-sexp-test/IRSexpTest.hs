{-# LANGUAGE ScopedTypeVariables #-}

-- | Round-trip acceptance tests for the troupe-ir-sexp format.
--
-- This suite checks the round-trip law R1 (@parse . print = id@, modulo source
-- positions) over:
--
--   * the ir2raw-test constructor corpus (reused verbatim), and
--   * a hand-built corpus that additionally exercises every literal form,
--     every operator, every VarAccess form, and the structured DC-labels —
--     so that every constructor of every type in the spec appears in at least
--     one round-tripped program.
module Main (main) where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (testProperty, forAll, (===), withMaxSuccess, Property)

import           IRSexp (printProg, printProgWithPos, parseProg, erasePosProg)
import           Gen (genProg)
import           IR
import qualified Core
import           Basics (BinOp(..), UnaryOp(..), LibName(..))
import           DCLabels
import           RetCPS (VarName(..))
import           TroupePositionInfo (Located(..), PosInf(..), noLoc)

import           Util (mkV, mkVN, mkLInst, mkLTerm)
import qualified Expr
import qualified Inst
import qualified TR
import qualified Tree

------------------------------------------------------------
-- The round-trip check.
------------------------------------------------------------

roundTrip :: IRProgram -> Assertion
roundTrip p =
  let expected = erasePosProg p
      printed  = printProg p
  in case parseProg printed of
       Left err ->
         assertFailure ("parse error: " ++ err ++ "\n--- printed ---\n" ++ printed)
       Right actual ->
         if expected == actual
           then return ()
           else assertFailure
             ( "round-trip mismatch\n--- printed ---\n" ++ printed
               ++ "\n--- reparsed, re-printed ---\n" ++ printProg actual )

mkCases :: String -> [(String, IRProgram)] -> TestTree
mkCases name cases = testGroup name [ testCase n (roundTrip p) | (n, p) <- cases ]

main :: IO ()
main = defaultMain $ testGroup "troupe-ir-sexp round-trip"
  [ mkCases "ir2raw corpus: terminators"   TR.tcs
  , mkCases "ir2raw corpus: expressions"   Expr.tcs
  , mkCases "ir2raw corpus: instructions"  Inst.tcs
  , mkCases "ir2raw corpus: trees"         Tree.tcs
  , mkCases "all binary operators"         binOpCases
  , mkCases "all unary operators"          unOpCases
  , mkCases "variable access forms"        varAccessCases
  , mkCases "literal forms"                litCases
  , mkCases "dc-label forms"               dcLabelCases
  , mkCases "structural extras"            structuralCases
  , testProperty "parse (print p) == p (positions erased)"
      (withMaxSuccess 2000 propRoundTrip)
  , testProperty "parse (printWithPos p) == p (positions kept)"
      (withMaxSuccess 2000 propRoundTripPos)
  ]

-- | Over generated wrapped documents: parsing a printed program reproduces it
-- structurally, modulo source positions.
propRoundTrip :: Property
propRoundTrip =
  forAll genProg $ \p ->
    parseProg (printProg p) === Right (erasePosProg p)

-- | The same for the position-carrying printer, where nothing is lost: the
-- parsed program equals the original, positions included.
propRoundTripPos :: Property
propRoundTripPos =
  forAll genProg $ \p ->
    parseProg (printProgWithPos p) === Right p

------------------------------------------------------------
-- Program builders.
------------------------------------------------------------

progExpr :: IRExpr -> IRProgram
progExpr e =
  IRProgram [noLoc (FunDef (HFN "main") (mkVN "arg") [] body)]
  where body = BB [mkLInst (Assign (VN "r") e)] (mkLTerm (LibExport (mkV "r")))

progLit :: Core.Lit -> IRProgram
progLit lit = progExpr (Const lit)

lva :: VarAccess -> LVarAccess
lva = noLoc

------------------------------------------------------------
-- Operators (every constructor).
------------------------------------------------------------

binOpCases :: [(String, IRProgram)]
binOpCases =
  [ ("bin-" ++ show op, progExpr (Bin op (mkV "x") (mkV "y"))) | op <- Expr.binops ]

unOpCases :: [(String, IRProgram)]
unOpCases =
  [ ("un-" ++ show op, progExpr (Un op (mkV "x"))) | op <- Expr.unops ]

------------------------------------------------------------
-- Variable access forms.
------------------------------------------------------------

varAccessCases :: [(String, IRProgram)]
varAccessCases =
  [ ("VarLocal",      progExpr (Tuple [lva (VarLocal (VN "x"))] False))
  , ("VarEnv",        progExpr (Tuple [lva (VarEnv (VN "$env.y"))] False))
  , ("VarFunSelfRef", progExpr (Tuple [lva VarFunSelfRef] False))
  , ("mixed",         progExpr (Tuple [ lva (VarLocal (VN "a"))
                                      , lva (VarEnv (VN "b"))
                                      , lva VarFunSelfRef ] False))
  , ("variant",       progExpr (Tuple [lva (VarLocal (VN "x"))] True))
  ]

------------------------------------------------------------
-- Literal forms (every constructor of Lit / Numeric).
------------------------------------------------------------

litCases :: [(String, IRProgram)]
litCases =
  [ ("int-zero",       progLit (Core.LNumeric (Core.NumInt 0)))
  , ("int-neg",        progLit (Core.LNumeric (Core.NumInt (-42))))
  , ("int-unbounded",  progLit (Core.LNumeric (Core.NumInt (2 ^ (200 :: Int)))))
  , ("int-unbounded-neg", progLit (Core.LNumeric (Core.NumInt (negate (3 ^ (150 :: Int))))))
  , ("float-simple",   progLit (Core.LNumeric (Core.NumFloat 3.14)))
  , ("float-small",    progLit (Core.LNumeric (Core.NumFloat 1.0e-10)))
  , ("float-large",    progLit (Core.LNumeric (Core.NumFloat 6.022e23)))
  , ("float-neg",      progLit (Core.LNumeric (Core.NumFloat (-0.5))))
  , ("float-third",    progLit (Core.LNumeric (Core.NumFloat (1.0 / 3.0))))
  , ("float-zero",     progLit (Core.LNumeric (Core.NumFloat 0.0)))
  , ("string-simple",  progLit (Core.LString "hello world"))
  , ("string-empty",   progLit (Core.LString ""))
  , ("string-escapes", progLit (Core.LString "a\"b\\c\nd\te\rf"))
  , ("string-unicode", progLit (Core.LString "\x01\x1f\955snowman\9731"))
  , ("bool-true",      progLit (Core.LBool True))
  , ("bool-false",     progLit (Core.LBool False))
  , ("unit",           progLit Core.LUnit)
  , ("label-string",   progLit (Core.LLabel "{alice, bob}"))
  ]

------------------------------------------------------------
-- Structured DC-labels (every LabelComponent / LabelConst / LabelExp / LabelOp).
------------------------------------------------------------

tag :: String -> LabelExp
tag = TagExp

conf :: LabelComponent -> LabelComponent -> Core.Lit
conf c i = Core.LDCLabel (DCLabelExp (c, i))

ex :: LabelExp -> LabelComponent
ex = ExprComponent

dcLabelCases :: [(String, IRProgram)]
dcLabelCases =
  [ ("dclabel-true-false",  progLit (conf (ConstComponent LabelTrue) (ConstComponent LabelFalse)))
  , ("dclabel-false-true",  progLit (conf (ConstComponent LabelFalse) (ConstComponent LabelTrue)))
  , ("dclabel-tag-tag",     progLit (conf (ex (tag "alice")) (ex (tag "bob"))))
  , ("dclabel-conj",        progLit (conf (ex (OpExp Conj (tag "a") (tag "b")))
                                          (ConstComponent LabelFalse)))
  , ("dclabel-disj",        progLit (conf (ex (OpExp Disj (tag "alice") (tag "bob")))
                                          (ConstComponent LabelFalse)))
  , ("dclabel-nested",      progLit (conf (ex (OpExp Conj (tag "a")
                                                          (OpExp Disj (tag "b") (tag "c"))))
                                          (ex (OpExp Disj (OpExp Conj (tag "d") (tag "e"))
                                                          (tag "f")))))
  , ("dclabel-mixed-const", progLit (conf (ConstComponent LabelTrue) (ex (tag "root"))))
  , ("dclabel-tag-case",    progLit (conf (ex (tag "Alice")) (ex (tag "BOB"))))
  ]

------------------------------------------------------------
-- Structural extras: consts, mkclos, nested control flow, projections.
------------------------------------------------------------

progFun :: FunDef -> IRProgram
progFun f = IRProgram [noLoc f]

structuralCases :: [(String, IRProgram)]
structuralCases =
  [ ( "consts-nonempty"
    , progFun (FunDef (HFN "main") (mkVN "$$authorityarg")
        [ (VN "k1", Core.LNumeric (Core.NumInt 7))
        , (VN "k2", Core.LString "s")
        , (VN "k3", Core.LDCLabel (DCLabelExp (ConstComponent LabelTrue, ConstComponent LabelFalse)))
        ]
        (BB [mkLInst (Assign (VN "r") (Const Core.LUnit))]
            (mkLTerm (Ret (mkV "r"))))))
  , ( "mkclos-env-multi"
    , progFun (FunDef (HFN "main") (mkVN "arg") []
        (BB [ mkLInst (MkFunClosures
                        [ (VN "x", lva (VarLocal (VN "x")))
                        , (VN "y", lva (VarEnv (VN "y")))
                        , (VN "self", lva VarFunSelfRef) ]
                        [ (VN "f", HFN "f1"), (VN "g", HFN "g2") ]) ]
            (mkLTerm (LibExport (mkV "f"))))))
  , ( "if-nested"
    , progTerm (If (mkV "c")
        (BB [mkLInst (Assign (VN "a") (constInt 1))] (mkLTerm (Ret (mkV "a"))))
        (BB [mkLInst (Assign (VN "b") (constInt 2))] (mkLTerm (Ret (mkV "b")))))
    )
  , ( "stack-expand"
    , progTerm (StackExpand (VN "r")
        (BB [mkLInst (Assign (VN "t") (constInt 10))] (mkLTerm (Ret (mkV "t"))))
        (BB [mkLInst (Assign (VN "s") (Bin Plus (mkV "r") (mkV "r")))]
            (mkLTerm (Ret (mkV "s")))))
    )
  , ( "assert-else-error"
    , progTerm (AssertElseError (mkV "c")
        (BB [] (mkLTerm (Ret (mkV "c"))))
        (mkV "msg"))
    )
  , ( "proj-idx-max",  progExpr (ProjIdx (mkV "t") 2147483647) )
  , ( "proj-idx-zero", progExpr (ProjIdx (mkV "t") 0) )
  , ( "lib-and-base"
    , progFun (FunDef (HFN "main") (mkVN "arg") []
        (BB [ mkLInst (Assign (VN "a") (Base "$$authorityarg"))
            , mkLInst (Assign (VN "b") (Lib (LibName "string") "charAt")) ]
            (mkLTerm (LibExport (mkV "b")))))
    )
  , ( "tail-call",  progTerm (TailCall (mkV "f") (mkV "x")) )
  , ( "error-term", progTerm (Error (mkV "msg")) )
  , ( "with-record"
    , progExpr (WithRecord (mkV "base") [("f1", mkV "v1"), ("f2", mkV "v2")])
    )
  , ( "proj-field", progExpr (ProjField (mkV "rec") "some.field$name") )
  ]
  where
    progTerm tr =
      IRProgram [noLoc (FunDef (HFN "main") (mkVN "arg") [] (BB [] (mkLTerm tr)))]
    constInt n = Const (Core.LNumeric (Core.NumInt n))
