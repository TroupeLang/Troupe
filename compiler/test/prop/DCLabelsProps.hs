-- Property-based tests for the compiler-side DC-label lattice (DCLabels.hs).
--
-- Strategy: a brute-force semantic oracle over a small (5-tag) universe. For
-- monotone (negation-free) CNF, semantic implication is equivalent to clause
-- subsumption, so testing `cnfImplies` against the oracle is a real soundness
-- AND completeness witness, not just self-consistency.

module Main (main) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Data.List (subsequences, intercalate)
import Data.Char (toLower, toUpper)
import DCLabels

-- Small, all-lowercase universe: 5 tags => 32 assignments, cheap to enumerate.
tagUniverse :: [String]
tagUniverse = ["alice", "bob", "charlie", "dorothy", "eve"]

-- Generators ----------------------------------------------------------------

genLabelExp :: Int -> Gen LabelExp
genLabelExp 0 = TagExp <$> elements tagUniverse
genLabelExp n = oneof
  [ TagExp <$> elements tagUniverse
  , OpExp <$> elements [Conj, Disj] <*> sub <*> sub ]
  where sub = genLabelExp (n `div` 2)

instance Arbitrary LabelExp where
  arbitrary = sized (\n -> genLabelExp (min n 8))
  shrink (OpExp _ a b) = [a, b]
  shrink _             = []

-- Boolean semantics. `ts` is the set of tags assigned True. ------------------

evalExp :: [String] -> LabelExp -> Bool
evalExp ts (TagExp t)       = map toLower t `elem` ts
evalExp ts (OpExp Conj a b) = evalExp ts a && evalExp ts b
evalExp ts (OpExp Disj a b) = evalExp ts a || evalExp ts b

-- A clause (disjunction) is satisfied if any of its tags is True; a CNF
-- (conjunction of clauses) is satisfied if every clause is satisfied. Empty
-- CNF is True; a CNF containing an empty clause is False.
evalCNF :: [String] -> CNF -> Bool
evalCNF ts (CNF ds) = all (\(DisjTags d) -> any (`elem` ts) d) ds

allAssignments :: [[String]]
allAssignments = subsequences tagUniverse

-- Semantic implication oracle: x |= y iff every assignment satisfying x also
-- satisfies y.
semImplies :: CNF -> CNF -> Bool
semImplies x y = all (\ts -> not (evalCNF ts x) || evalCNF ts y) allAssignments

-- Properties ----------------------------------------------------------------

-- labelExpToCNF preserves boolean semantics on every assignment.
prop_cnf_semantics :: LabelExp -> Property
prop_cnf_semantics e = forAll (sublistOf tagUniverse) $ \ts ->
  evalExp ts e === evalCNF ts (labelExpToCNF e)

-- cnfImplies agrees exactly with the semantic implication oracle
-- (soundness AND completeness of clause subsumption on monotone CNF).
prop_implies_semantic :: LabelExp -> LabelExp -> Property
prop_implies_semantic e1 e2 =
  let c1 = labelExpToCNF e1
      c2 = labelExpToCNF e2
  in cnfImplies c1 c2 === semImplies c1 c2

prop_implies_refl :: LabelExp -> Bool
prop_implies_refl e = let c = labelExpToCNF e in cnfImplies c c

prop_implies_trans :: LabelExp -> LabelExp -> LabelExp -> Property
prop_implies_trans e1 e2 e3 =
  let c1 = labelExpToCNF e1
      c2 = labelExpToCNF e2
      c3 = labelExpToCNF e3
  in cnfImplies c1 c2 && cnfImplies c2 c3 ==> cnfImplies c1 c3

prop_cnfEq_refl :: LabelExp -> Bool
prop_cnfEq_refl e = let c = labelExpToCNF e in cnfEq c c

prop_cnfEq_sym :: LabelExp -> LabelExp -> Property
prop_cnfEq_sym e1 e2 =
  let c1 = labelExpToCNF e1
      c2 = labelExpToCNF e2
  in cnfEq c1 c2 === cnfEq c2 c1

prop_cnfEq_trans :: LabelExp -> LabelExp -> LabelExp -> Property
prop_cnfEq_trans e1 e2 e3 =
  let c1 = labelExpToCNF e1
      c2 = labelExpToCNF e2
      c3 = labelExpToCNF e3
  in cnfEq c1 c2 && cnfEq c2 c3 ==> cnfEq c1 c3

-- Conjunction is the lattice meet: its CNF is satisfied exactly when both
-- operands are.
prop_conj_is_meet_sem :: LabelExp -> LabelExp -> Property
prop_conj_is_meet_sem e1 e2 = forAll (sublistOf tagUniverse) $ \ts ->
  let c1 = labelExpToCNF e1
      c2 = labelExpToCNF e2
  in evalCNF ts (labelExpToCNF (OpExp Conj e1 e2))
       === (evalCNF ts c1 && evalCNF ts c2)

-- Disjunction is the lattice join.
prop_disj_is_join_sem :: LabelExp -> LabelExp -> Property
prop_disj_is_join_sem e1 e2 = forAll (sublistOf tagUniverse) $ \ts ->
  let c1 = labelExpToCNF e1
      c2 = labelExpToCNF e2
  in evalCNF ts (labelExpToCNF (OpExp Disj e1 e2))
       === (evalCNF ts c1 || evalCNF ts c2)

-- V1 label helpers ----------------------------------------------------------

v1FromTags :: [String] -> String
v1FromTags ts = "{" ++ intercalate ", " ts ++ "}"

-- A base tag list and an order/case/duplication variant of it that must be
-- V1-equal.
genV1Variant :: Gen (String, String)
genV1Variant = do
  base    <- sublistOf tagUniverse
  dups    <- sublistOf base
  shuffled <- shuffle (base ++ dups)
  recased <- mapM randomCase shuffled
  return (v1FromTags base, v1FromTags recased)
  where
    randomCase s = do
      up <- arbitrary
      return $ if up then map toUpper s else s

genV1 :: Gen String
genV1 = v1FromTags <$> sublistOf tagUniverse

-- Permuting, duplicating, and re-casing a V1 label's principals does not
-- change its identity.
prop_v1_order_insensitive :: Property
prop_v1_order_insensitive = forAll genV1Variant $ \(l1, l2) ->
  v1LabelEq l1 l2

-- V1 string equality agrees with DC-label equality of the desugared forms.
prop_v1_dc_agreement :: Property
prop_v1_dc_agreement =
  forAll genV1 $ \l1 ->
  forAll genV1 $ \l2 ->
    v1LabelEq l1 l2
      === dcLabelEq (v1LabelToDCLabelExp l1) (v1LabelToDCLabelExp l2)

-- Main ----------------------------------------------------------------------

main :: IO ()
main = defaultMain $ localOption (QuickCheckTests 1000) $
  testGroup "DCLabels properties"
    [ testProperty "labelExpToCNF preserves semantics" prop_cnf_semantics
    , testProperty "cnfImplies == semantic implication" prop_implies_semantic
    , testProperty "cnfImplies reflexive"               prop_implies_refl
    -- The transitivity antecedent (both implications hold) is rare under
    -- independent random generation, so raise the discard ratio to reach
    -- 1000 valid cases rather than giving up at the default ceiling.
    , localOption (QuickCheckMaxRatio 2000) $
        testProperty "cnfImplies transitive"            prop_implies_trans
    , testProperty "cnfEq reflexive"                    prop_cnfEq_refl
    , testProperty "cnfEq symmetric"                    prop_cnfEq_sym
    , localOption (QuickCheckMaxRatio 2000) $
        testProperty "cnfEq transitive"                 prop_cnfEq_trans
    , testProperty "conjunction is meet"                prop_conj_is_meet_sem
    , testProperty "disjunction is join"                prop_disj_is_join_sem
    , testProperty "v1 order/case/dup insensitive"      prop_v1_order_insensitive
    , testProperty "v1 equality agrees with dcLabelEq"  prop_v1_dc_agreement
    ]
