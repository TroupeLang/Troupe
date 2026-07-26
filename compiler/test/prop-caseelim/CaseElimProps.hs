-- Property-based (oracle / differential) tests for pattern-match compilation,
-- i.e. CaseElimination.trans, which lowers the pattern-bearing Direct AST into
-- the pattern-free DirectWOPats AST.
--
-- Strategy. `trans` is a pure Except-monad function, so we exercise it entirely
-- in-process with no JS emission and no runtime:
--
--   1. Generate a random source pattern `p` (over the covered forms below) and a
--      random runtime value `v` drawn from a small Haskell value model `V`.
--   2. Build a minimal source program whose body is
--          case $scrut of p => (x1, ..., xk)   (* xi = binders of p, L-to-R *)
--      `trans` auto-appends an Error fallback for the non-matching branch.
--   3. Run `trans CompileMode.Library` (Library => no authority-arg wrapper) and
--      interpret the resulting pattern-free term with a small evaluator over `V`,
--      starting from the environment { "$scrut" -> v }. Reaching an `Error`
--      (or a failed `AssertElseError`) means "no match"; otherwise the body
--      tuple yields the bound values in binder order.
--   4. Compare against an INDEPENDENT reference matcher `refMatch`, written from
--      what each pattern *means* to a programmer (not derived from the compiler):
--      it must agree on (a) match vs. no-match and (b) the resulting bindings.
--
-- The interpreter implements exactly the closed op set that `compilePattern` /
-- `transTerm` can emit for a Case over the covered patterns: terms Var, Lit,
-- Let (ValDecl), If, AssertElseError, Error, ProjIdx, ProjField, Tuple, List,
-- ListCons; unary IsList/IsTuple/IsRecord/Head/Tail/ListLength/TupleLength/
-- RecordSize; binary Eq/And/Gt/HasField. `And` is modeled short-circuiting,
-- which is faithful: Core.hs lowers `Bin And a b` to `if a then b else false`.
--
-- Covered pattern forms: Wildcard, VarPattern, ValPattern (Int/Bool/String/Unit/
-- Atom literals), TuplePattern, ConsPattern, ListPattern, RecordPattern in BOTH
-- WildcardMatch and ExactMatch modes, arbitrarily nested (records also cover
-- punning fields `{f}` and sub-pattern fields `{f = p}`).
--
-- Deferred (v1 does NOT generate these):
--   * AtPattern  -- matches a value's *label* via LevelOf; needs a
--                   label-carrying value model. Follow-up.
--   * ErrorPattern -- parser error-recovery placeholder, not real surface syntax.

module Main (main) where

import Test.Tasty
import Test.Tasty.QuickCheck

import Control.Monad (foldM, replicateM)
import Control.Monad.Except (runExcept)
import Data.List (nub, intercalate)

import qualified Direct as S
import Direct (DeclPattern(..), RecordPatternMode(..))
import qualified DirectWOPats as T
import Basics
import qualified CompileMode
import TroupePositionInfo (Located(..), unLoc, PosInf(..))
import CaseElimination (trans)

-- ---------------------------------------------------------------------------
-- Value model
-- ---------------------------------------------------------------------------

data V = VInt Integer
       | VBool Bool
       | VString String
       | VUnit
       | VTuple [V]
       | VList [V]
       | VRecord [(String, V)]   -- keys kept unique (map semantics)
  deriving (Eq, Show)

isList, isTuple, isRecord :: V -> Bool
isList   (VList _)   = True; isList   _ = False
isTuple  (VTuple _)  = True; isTuple  _ = False
isRecord (VRecord _) = True; isRecord _ = False

litToV :: S.Lit -> V
litToV (S.LNumeric (S.NumInt n)) = VInt n
litToV (S.LBool b)               = VBool b
litToV S.LUnit                   = VUnit
litToV (S.LString s)             = VString s
litToV l = error ("litToV: literal not in generator domain: " ++ show l)

-- ---------------------------------------------------------------------------
-- Reference matcher (the oracle) -- encodes INTENDED pattern semantics,
-- independently of compilePattern. Bindings are returned in the same
-- left-to-right DFS order as `binders`.
-- ---------------------------------------------------------------------------

refMatch :: S.DeclPattern -> V -> Maybe [(String, V)]
refMatch (VarPattern x) v          = Just [(x, v)]
refMatch Wildcard       _          = Just []
refMatch (ValPattern lit) v        = if v == litToV lit then Just [] else Nothing
refMatch (TuplePattern ps) (VTuple xs)
  | length ps == length xs         = concatMatches (zipWith refMatchL ps xs)
refMatch (TuplePattern _) _        = Nothing
refMatch (ListPattern ps) (VList xs)
  | length ps == length xs         = concatMatches (zipWith refMatchL ps xs)
refMatch (ListPattern _) _         = Nothing
refMatch (ConsPattern h t) (VList (x:xs)) = do
  bh <- refMatchL h x
  bt <- refMatchL t (VList xs)
  Just (bh ++ bt)
refMatch (ConsPattern _ _) _       = Nothing
refMatch (RecordPattern fs mode) (VRecord kvs) =
  let sizeOk = case mode of
                 ExactMatch    -> length kvs == length fs
                 WildcardMatch -> True
  in if sizeOk then matchFields fs kvs else Nothing
refMatch (RecordPattern _ _) _     = Nothing
refMatch _ _                       = Nothing   -- AtPattern / ErrorPattern (not generated)

refMatchL :: S.LDeclPattern -> V -> Maybe [(String, V)]
refMatchL lp = refMatch (unLoc lp)

concatMatches :: [Maybe [(String, V)]] -> Maybe [(String, V)]
concatMatches ms = concat <$> sequence ms

matchFields :: [(FieldName, Maybe S.LDeclPattern)] -> [(String, V)] -> Maybe [(String, V)]
matchFields [] _ = Just []
matchFields ((f, mp) : rest) kvs =
  case lookup f kvs of
    Nothing -> Nothing
    Just fv -> do
      b1 <- case mp of
              Nothing -> Just [(f, fv)]     -- punning field binds `f`
              Just lp -> refMatchL lp fv
      b2 <- matchFields rest kvs
      Just (b1 ++ b2)

-- User-visible binder names, left-to-right DFS. Must match refMatch's order and
-- the order of the body tuple we hand to `trans`. Excludes the internal
-- $wildcard / $decltemp names the compiler introduces.
binders :: S.DeclPattern -> [String]
binders (VarPattern x)        = [x]
binders Wildcard              = []
binders (ValPattern _)        = []
binders (TuplePattern ps)     = concatMap (binders . unLoc) ps
binders (ListPattern ps)      = concatMap (binders . unLoc) ps
binders (ConsPattern h t)     = binders (unLoc h) ++ binders (unLoc t)
binders (RecordPattern fs _)  = concatMap fb fs
  where fb (f, Nothing) = [f]
        fb (_, Just lp) = binders (unLoc lp)
binders (AtPattern lp _)      = binders (unLoc lp)
binders ErrorPattern          = []

-- ---------------------------------------------------------------------------
-- Interpreter for the pattern-free DirectWOPats term
-- ---------------------------------------------------------------------------

data EvalErr = NoMatch | Stuck String
  deriving (Show)

type Env = [(String, V)]

eval :: Env -> T.LTerm -> Either EvalErr V
eval env (Loc _ t) = evalT env t

evalT :: Env -> T.Term -> Either EvalErr V
evalT env (T.Var x)  = maybe (Left (Stuck ("unbound var " ++ x))) Right (lookup x env)
evalT _   (T.Lit l)  = Right (litTToV l)
evalT env (T.Let decls body) = do
  env' <- foldM bindDecl env decls
  eval env' body
  where bindDecl e (T.ValDecl x lt) = do v <- eval e lt; Right ((x, v) : e)
        bindDecl _ (T.FunDecs _)    = Left (Stuck "unexpected FunDecs in compiled pattern")
evalT env (T.If c a b) = do
  cv <- eval env c
  case cv of
    VBool True  -> eval env a
    VBool False -> eval env b
    _           -> Left (Stuck "non-boolean If condition")
evalT env (T.AssertElseError c a _) = do
  cv <- eval env c
  case cv of
    VBool True  -> eval env a
    VBool False -> Left NoMatch
    _           -> Left (Stuck "non-boolean assertion condition")
evalT _   (T.Error _) = Left NoMatch
evalT env (T.ProjIdx e i) = do
  v <- eval env e
  case v of
    VTuple xs | fromIntegral i < length xs -> Right (xs !! fromIntegral i)
    _ -> Left (Stuck "ProjIdx on non-tuple or out of range")
evalT env (T.ProjField e f) = do
  v <- eval env e
  case v of
    VRecord kvs -> maybe (Left (Stuck ("ProjField missing " ++ f))) Right (lookup f kvs)
    _ -> Left (Stuck "ProjField on non-record")
evalT env (T.Tuple es _) = VTuple <$> mapM (eval env) es
evalT env (T.List es)  = VList  <$> mapM (eval env) es
evalT env (T.ListCons h t) = do
  hv <- eval env h
  tv <- eval env t
  case tv of
    VList xs -> Right (VList (hv : xs))
    _        -> Left (Stuck "ListCons with non-list tail")
evalT env (T.Un op e) = eval env e >>= evalUn op
-- Short-circuiting And: faithful to Core.hs lowering `a && b` => `if a then b else false`.
evalT env (T.Bin And a b) = do
  av <- eval env a
  case av of
    VBool False -> Right (VBool False)
    VBool True  -> do
      bv <- eval env b
      case bv of VBool _ -> Right bv; _ -> Left (Stuck "And rhs non-boolean")
    _ -> Left (Stuck "And lhs non-boolean")
evalT env (T.Bin op a b) = do
  av <- eval env a
  bv <- eval env b
  evalBin op av bv
evalT _ _ = Left (Stuck "unexpected compiled term")

evalUn :: UnaryOp -> V -> Either EvalErr V
evalUn IsList   v = Right (VBool (isList v))
evalUn IsTuple  v = Right (VBool (isTuple v))
evalUn IsRecord v = Right (VBool (isRecord v))
evalUn Head (VList (x:_))  = Right x
evalUn Head _              = Left (Stuck "Head of non-list or empty list")
evalUn Tail (VList (_:xs)) = Right (VList xs)
evalUn Tail _              = Left (Stuck "Tail of non-list or empty list")
evalUn ListLength  (VList xs)   = Right (VInt (toInteger (length xs)))
evalUn ListLength  _            = Left (Stuck "ListLength of non-list")
evalUn TupleLength (VTuple xs)  = Right (VInt (toInteger (length xs)))
evalUn TupleLength _            = Left (Stuck "TupleLength of non-tuple")
evalUn RecordSize  (VRecord kvs) = Right (VInt (toInteger (length kvs)))
evalUn RecordSize  _            = Left (Stuck "RecordSize of non-record")
evalUn op _ = Left (Stuck ("unsupported unary op " ++ show op))

evalBin :: BinOp -> V -> V -> Either EvalErr V
evalBin Eq a b = Right (VBool (a == b))     -- deep equality incl. type
evalBin Gt (VInt a) (VInt b) = Right (VBool (a > b))
evalBin Gt _ _ = Left (Stuck "Gt on non-integers")
evalBin HasField (VRecord kvs) (VString f) = Right (VBool (f `elem` map fst kvs))
evalBin HasField _ _ = Left (Stuck "HasField with wrong argument shapes")
evalBin op _ _ = Left (Stuck ("unsupported binary op " ++ show op))

litTToV :: T.Lit -> V
litTToV (T.LNumeric (T.NumInt n)) = VInt n
litTToV (T.LString s)             = VString s
litTToV (T.LBool b)               = VBool b
litTToV T.LUnit                   = VUnit
litTToV l = error ("litTToV: unexpected literal " ++ show l)

-- ---------------------------------------------------------------------------
-- Program construction and the property
-- ---------------------------------------------------------------------------

buildProg :: S.LDeclPattern -> S.Prog
buildProg lp =
  let body   = Loc NoPos (S.Tuple [ Loc NoPos (S.Var x) | x <- binders (unLoc lp) ] False)
      scrut  = Loc NoPos (S.Var "$scrut")
      caseE  = Loc NoPos (S.Case scrut [(lp, body)])
  in S.Prog (Imports []) [] caseE

prop_match :: Property
prop_match = forAllShrinkShow genCase shrinkCase showCase check
  where
    showCase (lp, v) = "pattern: " ++ showPat (unLoc lp) ++ " | value: " ++ show v
    check (lp, v) =
      case runExcept (trans CompileMode.Library (buildProg lp)) of
        Left err -> counterexample ("trans failed to compile: " ++ err) False
        Right (T.Prog _ term) ->
          let ref = refMatch (unLoc lp) v
              got = eval [("$scrut", v)] term
          in agree lp v ref got

agree :: S.LDeclPattern -> V -> Maybe [(String, V)] -> Either EvalErr V -> Property
agree lp v ref got =
  case (ref, got) of
    (Nothing, Left NoMatch) ->
      label "no-match" (property True)
    (Just binds, Right (VTuple vals)) ->
      label "match" $
        counterexample (ctx ++ "\nref bindings: " ++ show binds
                            ++ "\ncompiled vals: " ++ show vals)
          (map snd binds == vals)
    (Nothing, Left (Stuck s)) ->
      counterexample (ctx ++ "\nref=no-match but interpreter got stuck: " ++ s) False
    (Nothing, Right val) ->
      counterexample (ctx ++ "\nref=no-match but compiled MATCHED with: " ++ show val) False
    (Just binds, Left NoMatch) ->
      counterexample (ctx ++ "\nref MATCHED " ++ show binds ++ " but compiled: no match") False
    (Just binds, Left (Stuck s)) ->
      counterexample (ctx ++ "\nref MATCHED " ++ show binds ++ " but interpreter stuck: " ++ s) False
    (Just binds, Right other) ->
      counterexample (ctx ++ "\nref MATCHED " ++ show binds
                          ++ " but compiled body is not a tuple: " ++ show other) False
  where ctx = "pattern: " ++ showPat (unLoc lp) ++ "\nvalue:   " ++ show v

-- Readable rendering of a source pattern for counterexamples (DeclPattern has
-- no Show instance).
showPat :: S.DeclPattern -> String
showPat (VarPattern x)   = x
showPat Wildcard         = "_"
showPat (ValPattern l)   = showLit l
showPat (TuplePattern ps) = "(" ++ intercalate ", " (map (showPat . unLoc) ps) ++ ")"
showPat (ListPattern ps)  = "[" ++ intercalate ", " (map (showPat . unLoc) ps) ++ "]"
showPat (ConsPattern h t) = "(" ++ showPat (unLoc h) ++ " :: " ++ showPat (unLoc t) ++ ")"
showPat (RecordPattern fs mode) =
  "{" ++ intercalate ", " (map showField fs ++ dots) ++ "}"
  where showField (f, Nothing) = f
        showField (f, Just p)  = f ++ " = " ++ showPat (unLoc p)
        dots = case mode of WildcardMatch -> [".."]; ExactMatch -> []
showPat (AtPattern p l)  = showPat (unLoc p) ++ " @ " ++ l
showPat ErrorPattern     = "<error>"

showLit :: S.Lit -> String
showLit (S.LNumeric (S.NumInt n)) = show n
showLit (S.LBool b)   = if b then "true" else "false"
showLit S.LUnit       = "()"
showLit (S.LString s) = show s
showLit l             = "<lit:" ++ show l ++ ">"

-- ---------------------------------------------------------------------------
-- Generators
-- ---------------------------------------------------------------------------

genCase :: Gen (S.LDeclPattern, V)
genCase = do
  shape <- sized (\n -> genShape (min (n `div` 2) 4))
  let lp = snd (relabelP 0 shape)
  v <- genValue (unLoc lp)
  return (lp, v)

-- Pattern shapes; binder / field names are placeholders filled by `relabel`.
genShape :: Int -> Gen S.LDeclPattern
genShape d = Loc NoPos <$> genShape' d

genShape' :: Int -> Gen S.DeclPattern
genShape' d
  | d <= 0    = leaf
  | otherwise = frequency
      [ (3, leaf)
      , (2, TuplePattern <$> genChildren (genShape (d - 1)))
      , (2, ListPattern  <$> genChildren (genShape (d - 1)))
      , (2, ConsPattern  <$> genShape (d - 1) <*> genListTail (d - 1))
      , (2, genRecord d) ]
  where
    leaf = frequency
      [ (2, pure (VarPattern ""))
      , (1, pure Wildcard)
      , (2, ValPattern <$> genLit) ]
    genRecord dd = do
      n    <- choose (0, 3)
      mode <- elements [WildcardMatch, ExactMatch]
      fs   <- replicateM n (genField (dd - 1))
      return (RecordPattern fs mode)
    genField dd = do
      mp <- frequency [ (1, pure Nothing), (2, Just <$> genShape dd) ]
      return ("", mp)

genChildren :: Gen a -> Gen [a]
genChildren g = do n <- choose (0, 3); replicateM n g

-- List-compatible patterns for cons tails (so a matching tail *list* exists).
genListTail :: Int -> Gen S.LDeclPattern
genListTail d
  | d <= 0    = Loc NoPos <$> frequency
      [ (2, pure (VarPattern "")), (1, pure Wildcard), (1, pure (ListPattern [])) ]
  | otherwise = Loc NoPos <$> frequency
      [ (2, pure (VarPattern ""))
      , (1, pure Wildcard)
      , (2, ListPattern <$> genChildren (genShape (d - 1)))
      , (2, ConsPattern <$> genShape (d - 1) <*> genListTail (d - 1)) ]

genLit :: Gen S.Lit
genLit = elements
  [ S.LNumeric (S.NumInt (-1)), S.LNumeric (S.NumInt 0)
  , S.LNumeric (S.NumInt 1),    S.LNumeric (S.NumInt 2)
  , S.LBool True, S.LBool False, S.LUnit
  , S.LString "", S.LString "a", S.LString "b" ]

-- Assign globally-unique names to every VarPattern binder and every record
-- field, so all binders are distinct and no record has duplicate fields.
relabelP :: Int -> S.LDeclPattern -> (Int, S.LDeclPattern)
relabelP k (Loc l p) = let (k', p') = relabel k p in (k', Loc l p')

relabel :: Int -> S.DeclPattern -> (Int, S.DeclPattern)
relabel k (VarPattern _)     = (k + 1, VarPattern (freshName k))
relabel k Wildcard           = (k, Wildcard)
relabel k (ValPattern lit)   = (k, ValPattern lit)
relabel k (TuplePattern ps)  = let (k', ps') = relabelList k ps in (k', TuplePattern ps')
relabel k (ListPattern ps)   = let (k', ps') = relabelList k ps in (k', ListPattern ps')
relabel k (ConsPattern a b)  = let (k1, a') = relabelP k a
                                   (k2, b') = relabelP k1 b
                               in (k2, ConsPattern a' b')
relabel k (RecordPattern fs mode) = let (k', fs') = relabelFields k fs
                                    in (k', RecordPattern fs' mode)
relabel k p = (k, p)

relabelList :: Int -> [S.LDeclPattern] -> (Int, [S.LDeclPattern])
relabelList k []       = (k, [])
relabelList k (x : xs) = let (k1, x')  = relabelP k x
                             (k2, xs') = relabelList k1 xs
                         in (k2, x' : xs')

relabelFields :: Int -> [(FieldName, Maybe S.LDeclPattern)]
              -> (Int, [(FieldName, Maybe S.LDeclPattern)])
relabelFields k [] = (k, [])
relabelFields k ((_, mp) : rest) =
  let name = freshName k
      (k2, mp') = case mp of
                    Nothing -> (k + 1, Nothing)
                    Just lp -> let (kk, lp') = relabelP (k + 1) lp in (kk, Just lp')
      (k3, rest') = relabelFields k2 rest
  in (k3, (name, mp') : rest')

freshName :: Int -> String
freshName k = "n" ++ show k

-- Value generation for a (finalized, relabeled) pattern: a mix of guaranteed
-- matches, near-misses (a matching value structurally perturbed), and fully
-- random values -- so both matching and non-matching cases are exercised.
genValue :: S.DeclPattern -> Gen V
genValue p = do
  base <- frequency [ (3, genMatch p), (2, sized genArb) ]
  frequency [ (3, pure base), (2, perturb base) ]

genMatch :: S.DeclPattern -> Gen V
genMatch (VarPattern _)   = sized genArb
genMatch Wildcard         = sized genArb
genMatch (ValPattern lit) = pure (litToV lit)
genMatch (TuplePattern ps) = VTuple <$> mapM (genMatch . unLoc) ps
genMatch (ListPattern ps)  = VList  <$> mapM (genMatch . unLoc) ps
genMatch (ConsPattern h t) = do
  hv    <- genMatch (unLoc h)
  mrest <- genMatchList (unLoc t)
  case mrest of
    Just rest -> pure (VList (hv : rest))
    Nothing   -> sized genArb            -- no matching list for this tail; fallback
genMatch (RecordPattern fs mode) = do
  named  <- mapM fieldKV fs
  extras <- case mode of
              WildcardMatch -> genExtras (map fst named)
              ExactMatch    -> pure []
  pure (VRecord (named ++ extras))
  where fieldKV (f, Nothing) = do v <- sized genArb;             pure (f, v)
        fieldKV (f, Just lp) = do v <- genMatch (unLoc lp);      pure (f, v)
genMatch _ = sized genArb

-- Produce the element list of a VALUE `VList elems` that matches the pattern,
-- or Nothing when no matching list exists (Val/Tuple/Record tails).
genMatchList :: S.DeclPattern -> Gen (Maybe [V])
genMatchList (VarPattern _)   = Just <$> genArbList
genMatchList Wildcard         = Just <$> genArbList
genMatchList (ListPattern ps) = Just <$> mapM (genMatch . unLoc) ps
genMatchList (ConsPattern h t) = do
  hv    <- genMatch (unLoc h)
  mrest <- genMatchList (unLoc t)
  pure (fmap (hv :) mrest)
genMatchList _ = pure Nothing

genArbList :: Gen [V]
genArbList = do n <- choose (0, 3); replicateM n (sized genArb)

genExtras :: [String] -> Gen [(String, V)]
genExtras existing = do
  n <- choose (0, 2)
  ks <- replicateM n (elements ["x0", "x1", "x2"])
  let ks' = filter (`notElem` existing) (nub ks)
  mapM (\k -> do v <- sized genArb; pure (k, v)) ks'

genArb :: Int -> Gen V
genArb n
  | n <= 0    = leaf
  | otherwise = frequency
      [ (4, leaf)
      , (2, VTuple <$> smallList)
      , (2, VList  <$> smallList)
      , (2, genArbRecord (n `div` 2)) ]
  where
    leaf = oneof
      [ VInt    <$> elements [-1, 0, 1, 2]
      , VBool   <$> arbitrary
      , VString <$> elements ["", "a", "b"]
      , pure VUnit ]
    smallList = do k <- choose (0, 3); replicateM k (genArb (n `div` 2))
    genArbRecord m = do
      k  <- choose (0, 3)
      ks <- fmap nub (replicateM k (elements ["n0", "n1", "n2", "x0", "x1"]))
      kvs <- mapM (\key -> do v <- genArb m; pure (key, v)) ks
      pure (VRecord kvs)

-- Structural mutation of a value: produces near-misses (wrong arity/length,
-- changed literal, type mismatch, missing/extra record fields).
perturb :: V -> Gen V
perturb v = frequency [ (2, sized genArb), (3, structural v) ]
  where
    structural (VInt m)    = VInt <$> elements [m + 1, m - 1]
    structural (VBool b)   = pure (VBool (not b))
    structural (VString s) = pure (VString (s ++ "!"))
    structural VUnit       = sized genArb
    structural (VTuple xs) = frequency
      [ (1, VTuple <$> dropOne xs), (1, VTuple <$> addOne xs), (2, VTuple <$> perturbElem xs) ]
    structural (VList xs)  = frequency
      [ (1, VList <$> dropOne xs),  (1, VList <$> addOne xs),  (2, VList <$> perturbElem xs) ]
    structural (VRecord fs) = frequency
      [ (1, dropField fs), (1, addField fs), (2, perturbField fs) ]

    dropOne xs
      | null xs   = (: []) <$> sized genArb
      | otherwise = do i <- choose (0, length xs - 1); pure (deleteAt i xs)
    addOne xs = do v' <- sized genArb; i <- choose (0, length xs); pure (insertAt i v' xs)
    perturbElem xs
      | null xs   = (: []) <$> sized genArb
      | otherwise = do i <- choose (0, length xs - 1); x' <- perturb (xs !! i); pure (setAt i x' xs)

    dropField fs
      | null fs   = addField fs
      | otherwise = do i <- choose (0, length fs - 1); pure (VRecord (deleteAt i fs))
    addField fs =
      let cands = [ c | c <- ["z0", "z1", "z2", "z3", "z4"], c `notElem` map fst fs ]
      in case cands of
           []      -> pure (VRecord fs)
           (c : _) -> do v' <- sized genArb; pure (VRecord ((c, v') : fs))
    perturbField fs
      | null fs   = addField fs
      | otherwise = do
          i <- choose (0, length fs - 1)
          let (k, val) = fs !! i
          val' <- perturb val
          pure (VRecord (setAt i (k, val') fs))

-- ---------------------------------------------------------------------------
-- Shrinking
-- ---------------------------------------------------------------------------

shrinkCase :: (S.LDeclPattern, V) -> [(S.LDeclPattern, V)]
shrinkCase (lp, v) =
     [ (lp, v')  | v'  <- shrinkV v ]
  ++ [ (lp', v)  | lp' <- shrinkPat lp ]

shrinkV :: V -> [V]
shrinkV (VInt n)     = VInt <$> shrink n
shrinkV (VBool b)    = [VBool (not b)]
shrinkV (VString s)  = VString <$> shrink s
shrinkV VUnit        = []
shrinkV (VTuple xs)  = [VTuple ys | ys <- shrinkList shrinkV xs]
shrinkV (VList xs)   = [VList ys  | ys <- shrinkList shrinkV xs]
shrinkV (VRecord fs) =
     [ VRecord (deleteAt i fs) | i <- [0 .. length fs - 1] ]
  ++ [ VRecord (setAt i (k, v') fs) | (i, (k, v)) <- zip [0 ..] fs, v' <- shrinkV v ]

shrinkPat :: S.LDeclPattern -> [S.LDeclPattern]
shrinkPat (Loc l p) = map (Loc l) (shrinkP p)

shrinkP :: S.DeclPattern -> [S.DeclPattern]
shrinkP (TuplePattern ps) =
  map unLoc ps ++ [Wildcard] ++ [ TuplePattern ps' | ps' <- shrinkOneChild ps ]
shrinkP (ListPattern ps) =
  map unLoc ps ++ [Wildcard] ++ [ ListPattern ps' | ps' <- shrinkOneChild ps ]
shrinkP (ConsPattern h t) = [unLoc h, unLoc t, Wildcard]
shrinkP (RecordPattern fs mode) =
     [Wildcard]
  ++ [ RecordPattern (deleteAt i fs) mode | i <- [0 .. length fs - 1] ]
  ++ [ unLoc lp | (_, Just lp) <- fs ]
shrinkP (ValPattern _) = [Wildcard]
shrinkP _ = []

shrinkOneChild :: [S.LDeclPattern] -> [[S.LDeclPattern]]
shrinkOneChild ps =
  [ take i ps ++ [c'] ++ drop (i + 1) ps
  | i <- [0 .. length ps - 1], c' <- shrinkPat (ps !! i) ]

-- ---------------------------------------------------------------------------
-- Small list helpers
-- ---------------------------------------------------------------------------

deleteAt :: Int -> [a] -> [a]
deleteAt i xs = take i xs ++ drop (i + 1) xs

insertAt :: Int -> a -> [a] -> [a]
insertAt i x xs = take i xs ++ [x] ++ drop i xs

setAt :: Int -> a -> [a] -> [a]
setAt i x xs = take i xs ++ [x] ++ drop (i + 1) xs

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = defaultMain $ localOption (QuickCheckTests 2000) $
  testGroup "CaseElimination pattern-match compilation"
    [ testProperty "compiled decision logic agrees with reference matcher" prop_match ]
