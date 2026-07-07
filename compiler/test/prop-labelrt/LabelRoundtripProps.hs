-- Property-based round-trip tests for DC-label pretty-printing and parsing.
--
-- Property (Step 4g): for a random DCLabelExp `e`,
--     parse (render (ppDCLabelExpLit e))  is  dcLabelEq  to  e.
--
-- Strategy.
--
--   * Render.  `PP.render (ppDCLabelExpLit e)` produces the source spelling of
--     the label, wrapped in the literal delimiters, e.g. `` `< alice ; bob >` ``.
--
--   * Parse.  There is no standalone label parser; the happy grammar only parses
--     whole programs.  A DC-label literal is itself a valid Expr, and the top
--     production `Prog : ImportDecl AtomsDecl Expr` accepts an empty import list
--     and empty atoms declaration, so the rendered string *is* a minimal valid
--     program.  We call `Parser.parseProg "roundtrip" rendered :: Either String
--     Direct.Prog`.
--
--   * Extract.  The parsed program is
--         Prog (Imports []) (Atoms []) (Loc _ (Lit (LDCLabel e')))
--     so `extractLabel` walks the AST to the body literal and pulls out the
--     `DCLabelExp e'`.
--
--   * Compare.  `dcLabelEq e e'` is semantic (normalized CNF), so re-associated
--     or re-parenthesized operators are absorbed; we do not need structural
--     equality.
--
-- Two properties:
--
--   1. prop_roundtrip_expr -- over DCLabelExp whose BOTH components are
--      ExprComponent (tag expressions built from `&`/`|` over a small principal
--      pool).  This is the main deliverable and passes at 1000 cases.
--
--   2. prop_roundtrip_const -- over DCLabelExp that include a ConstComponent
--      (LabelTrue/LabelFalse).  `ppDCLabelExp` renders constant components with
--      the dimension-specific spellings the grammar accepts (`ppDCLabelExp` in
--      DCLabels.hs; ConfLabelExp/IntLabelExp in Parser.y):
--        confidentiality: LabelTrue -> #null-confidentiality,
--                         LabelFalse -> #root-confidentiality
--        integrity:       LabelTrue -> #null-integrity,
--                         LabelFalse -> #root-integrity
--      so const-containing labels round-trip too.  (Earlier the printer emitted
--      `show`'s display-only `#true`/`#false`, which the grammar rejected; that
--      asymmetry was fixed alongside this suite.)  prop_const_witness pins the
--      previously-broken case `DCLabelExp (ConstComponent LabelTrue,
--      ConstComponent LabelFalse)`, now round-tripping as
--      `` `<#null-confidentiality ; #root-integrity>` ``.

module Main (main) where

import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Text.PrettyPrint.HughesPJ as PP

import DCLabels
  ( DCLabelExp(..)
  , LabelComponent(..)
  , LabelExp(..)
  , LabelOp(..)
  , LabelConst(..)
  , ppDCLabelExpLit
  , dcLabelEq )
import Parser (parseProg)
import Direct (Prog(..), Term(..), Lit(..))
import TroupePositionInfo (unLoc)

-- ---------------------------------------------------------------------------
-- Render / parse / extract / compare
-- ---------------------------------------------------------------------------

-- Extract the DCLabelExp from a program whose body is a bare label literal.
extractLabel :: Prog -> Maybe DCLabelExp
extractLabel (Prog _ _ lterm) =
  case unLoc lterm of
    Lit (LDCLabel e') -> Just e'
    _                 -> Nothing

-- The round-trip check as a Property, with an informative counterexample.
roundtrip :: DCLabelExp -> Property
roundtrip e =
  let rendered = PP.render (ppDCLabelExpLit e)
  in case parseProg "roundtrip" rendered of
       Left err ->
         counterexample ("original: " ++ show e
                          ++ "\nrendered: " ++ rendered
                          ++ "\nparse error: " ++ err)
           (property False)
       Right prog ->
         case extractLabel prog of
           Nothing ->
             counterexample ("original: " ++ show e
                              ++ "\nrendered: " ++ rendered
                              ++ "\nno LDCLabel literal found in parsed program")
               (property False)
           Just e' ->
             counterexample ("original: " ++ show e
                              ++ "\nrendered: " ++ rendered
                              ++ "\nparsed:   " ++ show e')
               (dcLabelEq e e')

-- ---------------------------------------------------------------------------
-- Generators
-- ---------------------------------------------------------------------------

-- Small all-lowercase principal pool; each lexes as a VAR (not a keyword).
tagUniverse :: [String]
tagUniverse = ["alice", "bob", "charlie", "dorothy", "eve"]

genLabelExp :: Int -> Gen LabelExp
genLabelExp 0 = TagExp <$> elements tagUniverse
genLabelExp n = oneof
  [ TagExp <$> elements tagUniverse
  , OpExp <$> elements [Conj, Disj] <*> sub <*> sub ]
  where sub = genLabelExp (n `div` 2)

genExprComponent :: Gen LabelComponent
genExprComponent = ExprComponent <$> sized (\n -> genLabelExp (min n 6))

genConstComponent :: Gen LabelComponent
genConstComponent = ConstComponent <$> elements [LabelTrue, LabelFalse]

isConst :: LabelComponent -> Bool
isConst (ConstComponent _) = True
isConst _                  = False

-- Both components are tag expressions: the parseable-syntax fragment.
genExprDCLabel :: Gen DCLabelExp
genExprDCLabel = DCLabelExp <$> ((,) <$> genExprComponent <*> genExprComponent)

-- At least one component is a constant, so we always exercise the const case.
genConstDCLabel :: Gen DCLabelExp
genConstDCLabel = do
  c <- genAnyComponent
  i <- genAnyComponent
  if isConst c || isConst i
    then pure (DCLabelExp (c, i))
    else do
      k          <- genConstComponent
      onConf     <- arbitrary
      pure $ if onConf then DCLabelExp (k, i) else DCLabelExp (c, k)
  where genAnyComponent = frequency [(3, genExprComponent), (2, genConstComponent)]

-- ---------------------------------------------------------------------------
-- Shrinking
-- ---------------------------------------------------------------------------

shrinkLabelExp :: LabelExp -> [LabelExp]
shrinkLabelExp (TagExp _)      = []
shrinkLabelExp (OpExp o a b)   =
  [a, b]
  ++ [OpExp o a' b | a' <- shrinkLabelExp a]
  ++ [OpExp o a b' | b' <- shrinkLabelExp b]

-- ExprComponents shrink structurally; ConstComponents are kept as-is so a
-- const-containing witness stays const-containing (preserving its failure class).
shrinkComponent :: LabelComponent -> [LabelComponent]
shrinkComponent (ExprComponent e)  = ExprComponent <$> shrinkLabelExp e
shrinkComponent (ConstComponent _) = []

shrinkDC :: DCLabelExp -> [DCLabelExp]
shrinkDC (DCLabelExp (c, i)) =
     [ DCLabelExp (c', i) | c' <- shrinkComponent c ]
  ++ [ DCLabelExp (c, i') | i' <- shrinkComponent i ]

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

-- MAIN deliverable: expr-only labels round-trip.
prop_roundtrip_expr :: Property
prop_roundtrip_expr = forAllShrink genExprDCLabel shrinkDC roundtrip

-- Const-containing labels also round-trip: the printer emits the grammar's
-- #null-*/#root-* spellings for constant components.
prop_roundtrip_const :: Property
prop_roundtrip_const = forAllShrink genConstDCLabel shrinkDC roundtrip

-- Pin the previously-broken witness: `<#null-confidentiality ; #root-integrity>`
-- (rendered from LabelTrue/LabelFalse) now round-trips.
prop_const_witness :: Property
prop_const_witness =
  once $
    roundtrip (DCLabelExp (ConstComponent LabelTrue, ConstComponent LabelFalse))

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = defaultMain $ localOption (QuickCheckTests 1000) $
  testGroup "Label pretty-print / parse round-trip"
    [ testProperty "expr-component labels round-trip (dcLabelEq)" prop_roundtrip_expr
    , testProperty "const-component labels round-trip (#null-*/#root-* spellings)"
        prop_roundtrip_const
    , testProperty "const witness `<#null-confidentiality;#root-integrity>` round-trips"
        prop_const_witness
    ]
