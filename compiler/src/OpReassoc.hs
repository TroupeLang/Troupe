-- | Re-association: the translation from the parse-phase AST ('Surface') to
-- 'Direct'. Expressions arrive as flat operator chains; this pass rebuilds
-- each chain into the ordinary operator tree using a fixity environment —
-- built-in operators from the seed table below (which reproduces the
-- pre-chain grammar's precedence table exactly, differential-tested against
-- the whole corpus), user operators from fixity declarations and imports
-- (threaded by the caller; no declarations exist until the fixity-declaration
-- change lands, so every user operator currently fails with the
-- no-fixity-in-scope error).
--
-- Grouping is decided pairwise between adjacent operators (GHC's
-- compareFixity discipline): unequal levels bind by level; equal levels bind
-- left only if both are left-associative, right only if both are
-- right-associative, and are an error otherwise — which makes non-associative
-- adjacency (@1 = 2 = 3@) and mixed associativity at one level proper
-- compile errors. Prefix keyword operators ride the chains and bind the
-- maximal run of strictly tighter operators, reproducing e.g.
-- @not (x ^ y)@ for @not x ^ y@.
--
-- Design: @_dev_planning/custom-operators/design.md@ §4.3.
module OpReassoc (reassocProg) where

import           Basics
import qualified Direct as D
import qualified Surface as S
import           TroupePositionInfo (Located(..), PosInf(..))
import           Control.Monad.Except

--------------------------------------------------------------------------------
-- Fixity

data Assoc = ALeft | ARight | ANon deriving (Eq)

-- | (level, associativity) on the internal scale: user level n sits at 10n;
-- built-ins are seeded between, reproducing the declaration order of the
-- pre-chain precedence table.
type Fix = (Int, Assoc)

builtinFix :: S.ChainOp -> Maybe Fix
builtinFix (S.ChUser _) = Nothing
builtinFix S.ChCons     = Just (80, ARight)
builtinFix (S.ChBin op) = Just $ case op of
  And              -> (20, ALeft)
  Or               -> (20, ALeft)
  Eq               -> (40, ANon)
  Neq              -> (40, ANon)
  Lt               -> (40, ANon)
  Gt               -> (40, ANon)
  Le               -> (40, ANon)
  Ge               -> (40, ANon)
  BinAnd           -> (43, ALeft)
  BinOr            -> (43, ALeft)
  BinXor           -> (43, ALeft)
  BinShiftLeft     -> (46, ALeft)
  BinShiftRight    -> (46, ALeft)
  BinZeroShiftRight -> (46, ALeft)
  Plus             -> (60, ALeft)
  Minus            -> (60, ALeft)
  Mult             -> (70, ALeft)
  Div              -> (70, ALeft)
  IntDiv           -> (70, ALeft)
  Mod              -> (70, ALeft)
  RaisedTo         -> (84, ALeft)
  Concat           -> (90, ALeft)
  _                -> internalFixError op

-- HasField and LatticeJoin are compiler-generated, never parsed into chains.
internalFixError :: BinOp -> a
internalFixError op = error ("OpReassoc: no fixity for compiler-internal operator " ++ show op)

prefixLevel :: UnaryOp -> Int
prefixLevel IsTuple  = 85
prefixLevel IsList   = 86
prefixLevel IsRecord = 87
prefixLevel Not      = 88
prefixLevel u        = error ("OpReassoc: not a prefix chain operator: " ++ show u)

--------------------------------------------------------------------------------
-- Errors

type M = Except String

ppPos :: PosInf -> String
ppPos (SrcPosInf f l c) = f ++ ":" ++ show l ++ ":" ++ show c
ppPos (RTGen s)         = "<generated:" ++ s ++ ">"
ppPos NoPos             = "<unknown position>"

opSpelling :: S.ChainOp -> String
opSpelling (S.ChBin op) = show op
opSpelling S.ChCons     = "::"
opSpelling (S.ChUser v) = v

noFixityErr :: PosInf -> VarName -> M a
noFixityErr p v = throwError $
  ppPos p ++ ": operator '" ++ v ++ "' is used infix but has no fixity in scope\n"
  ++ "  declare it in the file header (e.g. 'infixl 6 " ++ v ++ "')\n"
  ++ "  or import it unqualified from a module that exports it"

incompatibleErr :: (PosInf, S.ChainOp, Fix) -> (PosInf, S.ChainOp, Fix) -> M a
incompatibleErr (p1, o1, (l1, a1)) (p2, o2, (l2, a2))
  | a1 == ANon && o1 `sameOp` o2 = throwError $
      ppPos p2 ++ ": operator '" ++ opSpelling o1
      ++ "' is non-associative; use parentheses to chain it"
  | otherwise = throwError $
      ppPos p2 ++ ": operators '" ++ opSpelling o1 ++ "' (" ++ ppPos p1
      ++ ") and '" ++ opSpelling o2
      ++ "' have the same precedence but incompatible associativity;"
      ++ " use parentheses"
  where sameOp a b = opSpelling a == opSpelling b

--------------------------------------------------------------------------------
-- The chain core: a shunting-yard whose tie cases are the errors.

data Resolution = ReduceFirst | ShiftSecond

resolve :: (PosInf, S.ChainOp, Fix) -> (PosInf, S.ChainOp, Fix) -> M Resolution
resolve o1@(_, _, (l1, a1)) o2@(_, _, (l2, a2))
  | l1 > l2                        = return ReduceFirst
  | l1 < l2                        = return ShiftSecond
  | ALeft  <- a1, ALeft  <- a2     = return ReduceFirst
  | ARight <- a1, ARight <- a2     = return ShiftSecond
  | otherwise                      = incompatibleErr o1 o2

data StackOp
    = SBin PosInf S.ChainOp Fix
    | SPre PosInf UnaryOp Int

reassocChain :: FixityLookup -> [S.ChainElem] -> M D.LTerm
reassocChain lookupFix = start
  where
    start elems = go [] [] elems

    go [t] [] [] = return t
    go ts (op : ops) [] = do ts' <- apply op ts
                             go ts' ops []
    go _ [] [] = malformed

    go ts ops (S.ChOperand t : rest) = do
      t' <- reassocLTerm lookupFix t
      go (t' : ts) ops rest
    go ts ops (S.ChPrefix p u : rest) =
      go ts (SPre p u (prefixLevel u) : ops) rest
    go ts ops (S.ChInfix p op : rest) = do
      f <- fixityOf p op
      (ts', ops') <- popFor (p, op, f) ts ops
      go ts' (SBin p op f : ops') rest

    -- Pop while the stack binds at least as tightly as the incoming operator.
    popFor o2@(_, _, (l2, _)) ts (top : ops)
      | SPre _ _ lp <- top, lp >= l2 = do   -- prefix takes its operand now
          ts' <- apply top ts
          popFor o2 ts' ops
      | SPre _ _ _ <- top = return (ts, top : ops)  -- prefix waits (not x ^ y)
      | SBin p1 op1 f1 <- top = do
          r <- resolve (p1, op1, f1) o2
          case r of
            ReduceFirst -> do ts' <- apply top ts
                              popFor o2 ts' ops
            ShiftSecond -> return (ts, top : ops)
    popFor _ ts ops = return (ts, ops)

    fixityOf p op = case builtinFix op of
      Just f  -> return f
      Nothing -> case op of
        S.ChUser v -> case lookupFix v of
          Just f  -> return f
          Nothing -> noFixityErr p v
        _ -> malformed

    apply (SBin p op _) (b : a : ts) = return (mkBinApp p op a b : ts)
    apply (SPre p u _)  (a : ts)     = return (Loc p (D.Un u a) : ts)
    apply _ _ = malformed

    malformed :: M a
    malformed = throwError "OpReassoc: malformed operator chain (parser invariant violated)"

mkBinApp :: PosInf -> S.ChainOp -> D.LTerm -> D.LTerm -> D.LTerm
mkBinApp p (S.ChBin op) a b = Loc p (D.Bin op a b)
mkBinApp p S.ChCons     a b = Loc p (D.ListCons a b)
mkBinApp p (S.ChUser v) a b =
  -- Curried application (the decided calling convention): a <+> b is
  -- ( <+> ) a b, the same shape juxtaposition application produces.
  Loc p (D.App (Loc p (D.Var v)) [a, b])

--------------------------------------------------------------------------------
-- Structural translation

-- | Fixity of a user operator by name; Nothing means no fixity in scope.
-- Currently always empty (no declarations exist yet); the fixity-declaration
-- and interface changes populate it.
type FixityLookup = VarName -> Maybe Fix

reassocProg :: S.Prog -> Except String D.Prog
reassocProg (S.Prog imports groups term) =
  D.Prog imports groups <$> reassocLTerm noUserFixities term
  where noUserFixities = const Nothing

reassocLTerm :: FixityLookup -> S.LTerm -> M D.LTerm
reassocLTerm env (Loc p t) = case t of
  S.OpChain elems      -> reassocChain env elems  -- carries its own positions
  _                    -> Loc p <$> reassocTerm env t

reassocTerm :: FixityLookup -> S.Term -> M D.Term
reassocTerm env tm = case tm of
  S.Lit l              -> return (D.Lit l)
  S.Var v              -> return (D.Var v)
  S.Abs lam            -> D.Abs <$> lambda lam
  S.Hnd h              -> D.Hnd <$> handler h
  S.App f as           -> D.App <$> go f <*> mapM go as
  S.Let ds b           -> D.Let <$> mapM decl ds <*> go b
  S.Case e alts        -> D.Case <$> go e <*> mapM alt alts
  S.If c a b           -> D.If <$> go c <*> go a <*> go b
  S.Tuple ts tag       -> flip D.Tuple tag <$> mapM go ts
  S.Record fs          -> D.Record <$> fields fs
  S.WithRecord r fs    -> D.WithRecord <$> go r <*> fields fs
  S.ProjField r f      -> flip D.ProjField f <$> go r
  S.ProjIdx r i        -> flip D.ProjIdx i <$> go r
  S.List ts            -> D.List <$> mapM go ts
  S.Seq ts             -> D.Seq <$> mapM go ts
  S.Neg t              -> D.Un UnMinus <$> go t
  S.OpChain _          -> throwError "OpReassoc: OpChain outside reassocLTerm"
  where
    go = reassocLTerm env
    alt (pat, body) = (,) pat <$> go body
    fields = mapM (\(n, mt) -> (,) n <$> mapM go mt)
    lambda (S.Lambda pats body) = D.Lambda pats <$> go body
    handler (S.Handler pat mpat g body) =
      D.Handler pat mpat <$> mapM go g <*> go body
    decl (S.ValDecl pat t)  = D.ValDecl pat <$> go t
    decl (S.FunDecs fds)    = D.FunDecs <$> mapM fdecl fds
    decl S.ErrorDecl        = return D.ErrorDecl
    fdecl (Loc p (S.FunDecl n lams)) =
      Loc p . D.FunDecl n <$> mapM lambda lams
