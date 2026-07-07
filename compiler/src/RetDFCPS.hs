{-# LANGUAGE TupleSections #-}
module RetDFCPS (transProg) where

import           Control.Monad.State.Lazy as State
import           qualified RetCPS as CPS
import           RetCPS
import qualified Core
import           TroupePositionInfo (Located(..), PosInf(..), GetPosInfo(..))

type S = State Integer

{--

A variant of the CPS transformation based on A.Kennedy's 2007 paper
which in itself is inspired by Danvy & Filinski's work for
our RetCPS language

--}

transFunDecs :: [Core.FunDecl] -> S [Located CPS.FunDef]
transFunDecs decls = do
  mapM transFunDecl decls

transFunDecl :: Core.FunDecl -> S (Located CPS.FunDef)
transFunDecl (Core.FunDecl fname (Core.Unary (Loc patPos pat) le) pos) = do
--  k <- freshK
  e' <- transExplicit le
  return $ Loc pos $ CPS.Fun (VN fname) (CPS.Unary (Loc patPos (VN pat)) e')
transFunDecl (Core.FunDecl fname (Core.Nullary le) pos) = do
--  k <- freshK
  e' <- transExplicit le
  return $ Loc pos $ CPS.Fun (VN fname) (CPS.Nullary e')

transProg :: Core.Prog -> CPS.Prog
transProg (Core.Prog imports atoms lt) =
  let pos = posInfo lt
  -- trans now passes LVarName; extract VarName for Halt
  in Prog atoms $ evalState (trans lt (\(Loc _ z) -> return $ Loc pos (Halt z))) 1


-- | Transform LFields in a context (non-explicit)
-- Now uses LVarName (Located VarName) for position tracking
transFields :: PosInf -> (CPS.LFields -> SimpleTerm) -> Core.LFields -> (CPS.LVarName -> S CPS.LKTerm) -> S CPS.LKTerm
transFields pos k fields context =
  transRecord fields [] context
    where
      transRecord [] acc ctx = do
          v <- freshV
          e' <- ctx (Loc pos v)
          return $ Loc pos $ LetSimple v (Loc pos (k (reverse acc))) e'
      transRecord ((f, lt):rest) acc ctx =
        trans lt (\lv -> transRecord rest ((f,lv):acc) ctx)


-- | Transform LFields in explicit context
-- Now uses LVarName (Located VarName) for position tracking
transFieldsExplicit :: PosInf -> (CPS.LFields -> SimpleTerm) -> Core.LFields -> S CPS.LKTerm
transFieldsExplicit pos k fields =
  iter fields []
    where iter [] acc = do
              v <- freshV
              return $ Loc pos $ LetSimple v (Loc pos (k (reverse acc))) (Loc pos (KontReturn v))
          iter ((f,lt):rest) acc  =
              trans lt (\lv -> iter rest ((f,lv):acc))


-- | Transform a Located Core term in explicit context
-- Produces Located CPS terms
transExplicit :: Core.LTerm -> S CPS.LKTerm
transExplicit (Loc pos (Core.Var (Core.RegVar x)))  = return $ Loc pos $ KontReturn (VN x)

transExplicit (Loc pos (Core.Var (Core.BaseName baseName))) = do
  x  <- freshV
  return $ Loc pos $ LetSimple x (Loc pos (Base baseName)) (Loc pos (KontReturn x))

transExplicit (Loc pos (Core.Var (Core.LibVar lib v))) = do
  x <- freshV
  return $ Loc pos $ LetSimple x (Loc pos (Lib lib v)) (Loc pos (KontReturn x))

transExplicit (Loc pos (Core.Lit lit)) = do
  x <- freshV
  -- Use position from Located wrapper, not from literal (which may be NoPos for non-numeric literals)
  return $ Loc pos $ LetSimple x (Loc pos (ValSimpleTerm (CPS.Lit lit))) (Loc pos (KontReturn x))

transExplicit (Loc pos (Core.Error lterm)) = do
  -- trans now passes LVarName; extract VarName for Error
  -- Position is now in the Located wrapper, not ErrorPosInf
  trans lterm (\(Loc _ v) -> return $ Loc pos (Error v))

transExplicit (Loc pos (Core.App le1 le2)) = do
  -- trans now passes LVarName; extract VarName for ApplyFun
  trans le1 (\(Loc _ x1) ->
    trans le2 (\(Loc _ x2) ->
      return $ Loc pos $ ApplyFun x1 x2))

transExplicit (Loc pos (Core.Bin op le1 le2)) = do
  x <- freshV
  -- trans now passes LVarName; use directly in Bin (which takes LVarName)
  trans le1 (\lv1 ->
    trans le2 (\lv2 ->
      return $ Loc pos $ LetSimple x (Loc pos (CPS.Bin op lv1 lv2)) (Loc pos (KontReturn x))))

transExplicit (Loc pos (Core.Un op le)) = do
  x <- freshV
  -- trans now passes LVarName; use directly in Un (which takes LVarName)
  trans le (\lv ->
      return $ Loc pos $ LetSimple x (Loc pos (CPS.Un op lv)) (Loc pos (KontReturn x)))

transExplicit (Loc pos (Core.Abs (Core.Unary (Loc xPos x) le))) = do
  f <- freshV
  e' <- transExplicit le
  return $ Loc pos $ LetSimple f (Loc pos (ValSimpleTerm (KAbs (Unary (Loc xPos (VN x)) e')))) (Loc pos (KontReturn f))

transExplicit (Loc pos (Core.Abs (Core.Nullary le))) = do
  f <- freshV
  e' <- transExplicit le
  return $ Loc pos $ LetSimple f (Loc pos (ValSimpleTerm (KAbs (Nullary e')))) (Loc pos (KontReturn f))

transExplicit (Loc pos (Core.Let (Core.ValDecl v le1) le2))  = do
  e2' <- transExplicit le2
  e1' <- transExplicit le1
  return $ Loc pos $ LetRet (Cont (VN v) e2') e1'

transExplicit (Loc pos (Core.Let (Core.FunDecs decs) le2))  = do
  decs' <- transFunDecs decs
  e2' <- transExplicit le2
  return $ Loc pos $ LetFun decs' e2'

transExplicit (Loc pos (Core.If le0 le1 le2))  = do
  e1' <- transExplicit le1
  e2' <- transExplicit le2
  -- trans now passes LVarName; extract VarName for If
  trans le0 (\(Loc _ z) -> return $ Loc pos $ If z e1' e2')

-- 2018-09-28: AA; gotta double check this part of
-- the translation
transExplicit (Loc pos (Core.AssertElseError le0 le1 le2)) = do
  e1' <- transExplicit le1
  -- trans now passes LVarName; extract VarName for AssertElseError
  -- Position is now in the Located wrapper, not ErrorPosInf
  trans le0 (\(Loc _ v0) ->
    trans le2 (\(Loc _ v2) ->
      return $ Loc pos $ AssertElseError v0 e1' v2))


transExplicit (Loc pos (Core.Tuple lts))  =
  transTuple lts []
  where
    -- Now uses LVarName for position tracking
    transTuple :: [Core.LTerm] -> [CPS.LVarName] -> S CPS.LKTerm
    transTuple [] acc  = do
      v <- freshV
      return $ Loc pos $ LetSimple v (Loc pos (Tuple (reverse acc))) (Loc pos (KontReturn v))
    transTuple (lt:rest) acc  =
      trans lt (\lv -> transTuple rest (lv:acc) )

transExplicit (Loc pos (Core.Record fields)) =
    transFieldsExplicit pos Record fields

transExplicit (Loc pos (Core.WithRecord le fields)) =
  -- trans now passes LVarName; use directly in WithRecord
  trans le (\lv -> transFieldsExplicit pos (WithRecord lv) fields)


transExplicit (Loc pos (Core.ProjField lt f))= do
  x <- freshV
  -- trans now passes LVarName; use directly in ProjField
  trans lt (\lv ->
    return $ Loc pos $ LetSimple x (Loc pos (CPS.ProjField lv f)) (Loc pos (KontReturn x)))

transExplicit (Loc pos (Core.ProjIdx lt idx)) = do
  x <- freshV
  -- trans now passes LVarName; use directly in ProjIdx
  trans lt (\lv ->
    return $ Loc pos $ LetSimple x (Loc pos (CPS.ProjIdx lv idx)) (Loc pos (KontReturn x)))

transExplicit (Loc pos (Core.List lts)) =
  transList lts []
  where
    -- Now uses LVarName for position tracking
    transList [] acc  = do
      v <- freshV
      return $ Loc pos $ LetSimple v (Loc pos (List (reverse acc))) (Loc pos (KontReturn v))
    transList (lt:rest) acc =
      trans lt (\lv -> transList rest (lv:acc))

transExplicit (Loc pos (Core.ListCons lh lt)) = do
  v <- freshV
  -- trans now passes LVarName; use directly in ListCons
  trans lh (\lvh -> trans lt (\lvt -> return $ Loc pos $ LetSimple v (Loc pos (ListCons lvh lvt)) (Loc pos (KontReturn v))))

-- | Transform a Located Core term with a continuation
-- Produces Located CPS terms
-- The continuation now receives LVarName (Located VarName) to preserve source positions
trans :: Core.LTerm -> (CPS.LVarName -> S CPS.LKTerm) -> S CPS.LKTerm

-- For variables, pass the position from the source location
trans (Loc pos (Core.Var (Core.RegVar x))) context = context (Loc pos (VN x))

trans (Loc pos (Core.Var (Core.BaseName baseName))) context = do
  x <- freshV
  kterm' <- context (Loc pos x)
  return $ Loc pos $ LetSimple x (Loc pos (Base baseName)) kterm'


trans (Loc pos (Core.Var (Core.LibVar lib v))) context = do
  x <- freshV
  kterm' <- context (Loc pos x)
  return $ Loc pos $ LetSimple x (Loc pos (Lib lib v)) kterm'


trans (Loc pos (Core.Lit lit)) context =
  do x <- freshV
     -- Use position from Located wrapper, not from literal (which may be NoPos for non-numeric literals)
     kterm' <- context (Loc pos x)
     return $ Loc pos $ LetSimple x (Loc pos (ValSimpleTerm (CPS.Lit lit))) kterm'

trans (Loc pos (Core.Error le)) context = do
  x  <- freshV
  kterm <- context (Loc pos x)
  -- Extract VarName from LVarName for Error
  -- Position is now in the Located wrapper, not ErrorPosInf
  trans le (\(Loc _ z) -> return $ Loc pos $ LetRet (Cont x kterm) (Loc pos (Error z)))

trans (Loc pos (Core.App le1 le2)) context = do
  x  <- freshV
  kterm <- context (Loc pos x)
  -- Extract VarName from LVarName for ApplyFun
  trans le1 (\(Loc _ z1) ->
    trans le2 (\(Loc _ z2) ->
      return $ Loc pos $ LetRet (Cont x kterm) (Loc pos (ApplyFun z1 z2))))

trans (Loc pos (Core.Bin op le1 le2)) context = do
  x <- freshV
  kterm <- context (Loc pos x)
  -- Use LVarName directly in Bin
  trans le1 (\lv1 ->
    trans le2 (\lv2 ->
      return $ Loc pos $ LetSimple x (Loc pos (CPS.Bin op lv1 lv2)) kterm))

trans (Loc pos (Core.Un op le)) context = do
  x <- freshV
  kterm <- context (Loc pos x)
  -- Use LVarName directly in Un
  trans le (\lv -> return $ Loc pos $ LetSimple x (Loc pos (CPS.Un op lv)) kterm)

trans (Loc pos (Core.Abs (Core.Unary (Loc xPos x) le))) context = do
  f <- freshV
  kterm <- context (Loc pos f)
  e' <- transExplicit le
  return $ Loc pos $ LetSimple f (Loc pos (ValSimpleTerm (KAbs (Unary (Loc xPos (VN x)) e')))) kterm

trans (Loc pos (Core.Abs (Core.Nullary le))) context = do
  f <- freshV
  kterm <- context (Loc pos f)
  e' <- transExplicit le
  return $ Loc pos $ LetSimple f (Loc pos (ValSimpleTerm (KAbs (Nullary e')))) kterm

trans (Loc pos (Core.Let (Core.ValDecl v le1) le2)) context = do
  e2' <- trans le2 context
  e1' <- transExplicit le1
  return $ Loc pos $ LetRet (Cont (VN v) e2') e1'

trans (Loc pos (Core.Let (Core.FunDecs decs) le2)) context = do
  decs' <- transFunDecs decs
  e2' <- trans le2 context
  return $ Loc pos $ LetFun decs' e2'

trans (Loc pos (Core.If le0 le1 le2)) context = do
  v <- freshV
  kterm <- context (Loc pos v)
  e1' <- transExplicit le1
  e2' <- transExplicit le2
  -- Extract VarName from LVarName for If
  trans le0 (\(Loc _ z) -> return $ Loc pos $ LetRet (Cont v kterm) (Loc pos (If z e1' e2')))


trans (Loc pos (Core.AssertElseError le0 le1 le2)) context = do
  x <- freshV
  kterm <- context (Loc pos x)
  e1' <- transExplicit le1
  -- Extract VarName from LVarName for AssertElseError
  -- Position is now in the Located wrapper, not ErrorPosInf
  trans le0 (\(Loc _ z) ->
    trans le2 (\(Loc _ z2) ->
      return $ Loc pos $ LetRet (Cont x kterm) (Loc pos (AssertElseError z e1' z2))))



trans (Loc pos (Core.Tuple lts)) context =
  transTuple lts [] context
  where
    -- Now uses LVarName for position tracking
    transTuple [] acc ctx = do
      v <- freshV
      e' <- ctx (Loc pos v)
      return $ Loc pos $ LetSimple v (Loc pos (Tuple (reverse acc))) e'
    transTuple (lt:rest) acc ctx =
      trans lt (\lv -> transTuple rest (lv:acc) ctx)

trans (Loc pos (Core.Record fields)) context = transFields pos Record fields context

trans (Loc pos (Core.WithRecord le fields)) context =
  -- Use LVarName directly in WithRecord
  trans le (\lv -> transFields pos (WithRecord lv) fields context )


trans (Loc pos (Core.ProjField lt f)) context = do
  x <- freshV
  kterm <- context (Loc pos x)
  -- Use LVarName directly in ProjField
  trans lt (\lv -> return $ Loc pos $ LetSimple x (Loc pos (CPS.ProjField lv f)) kterm)

trans (Loc pos (Core.ProjIdx lt idx)) context = do
  x <- freshV
  kterm <- context (Loc pos x)
  -- Use LVarName directly in ProjIdx
  trans lt (\lv -> return $ Loc pos $ LetSimple x (Loc pos (CPS.ProjIdx lv idx)) kterm)

trans (Loc pos (Core.List lts)) context =
  transList lts [] context
  where
    -- Now uses LVarName for position tracking
    transList [] acc ctx = do
      v <- freshV
      e' <- ctx (Loc pos v)
      return $ Loc pos $ LetSimple v (Loc pos (List (reverse acc))) e'
    transList (lt:rest) acc ctx =
      trans lt (\lv -> transList rest (lv:acc) ctx)

trans (Loc pos (Core.ListCons lh lt)) context = do
  v <- freshV
  e' <- context (Loc pos v)
  -- Use LVarName directly in ListCons
  trans lh (\lvh -> trans lt (\lvt -> return $ Loc pos $ LetSimple v (Loc pos (ListCons lvh lvt)) e'))


freshSymbol :: S String
freshSymbol = do
  n <- State.get
  put (n + 1)
  return $ ("gensym" ++ show n)

freshV :: S CPS.VarName
freshV = do
      s <- freshSymbol
      return $ VN s

-- freshK :: S KontName
-- freshK = do
--        s <- freshSymbol
--        return $ K s
