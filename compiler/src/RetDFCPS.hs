{-# LANGUAGE TupleSections #-}
module RetDFCPS (transProg) where

import           Basics
import           Control.Monad.State.Lazy as State
import           qualified RetCPS as CPS
import           RetCPS
import qualified Core

type S = State Integer

{--

A variant of the CPS transformation based on A.Kennedy's 2007 paper
which in itself is inspired by Danvy & Filinski's work for
our RetCPS language

--}

transFunDecs :: [Core.FunDecl] -> S [CPS.FunDef]
transFunDecs decls = do
  mapM transFunDecl decls

transFunDecl :: Core.FunDecl -> S CPS.FunDef
transFunDecl (Core.FunDecl fname (Core.Unary pat e)) = do
--  k <- freshK
  e' <- transExplicit e
  return $ CPS.Fun (VN fname) (CPS.Unary (VN pat) e')
transFunDecl (Core.FunDecl fname (Core.Nullary e)) = do
--  k <- freshK
  e' <- transExplicit e
  return $ CPS.Fun (VN fname) (CPS.Nullary e')

transProg :: Core.Prog -> CPS.Prog
transProg (Core.Prog imports atoms t) =
  Prog atoms $ evalState (trans t (\z -> return $ Halt z)) 1

transExplicit :: Core.Term -> S CPS.KTerm
transExplicit (Core.Var (Core.RegVar x))  = return $ KontReturn (VN x)

transExplicit (Core.Var (Core.BaseName baseName)) = do
  x  <- freshV
  return $ LetSimple x (Base baseName) (KontReturn x)

transExplicit (Core.Var (Core.LibVar lib v)) = do
  x <- freshV
  return $ LetSimple x (Lib lib v) (KontReturn x)

transExplicit (Core.Lit lit) = do
  x <- freshV
  return $ LetSimple x (ValSimpleTerm (CPS.Lit lit)) (KontReturn x)

transExplicit (Core.Error term p) = do
  trans term (\v -> return $ Error v p)

transExplicit (Core.App e1 e2) = do
  trans e1 (\x1 ->
    trans e2 (\x2 ->
      return $ ApplyFun x1 x2 ))

transExplicit (Core.Bin op e1 e2) = do
  x <- freshV
  trans e1 (\x1 ->
    trans e2 (\x2 ->
      return $ LetSimple x (CPS.Bin op x1 x2) (KontReturn x)))

transExplicit (Core.Un op e) = do
  x <- freshV
  trans e (\x' ->
      return $ LetSimple x (CPS.Un op x') (KontReturn x))

transExplicit (Core.Abs (Core.Unary x e)) = do
  f <- freshV
  e' <- transExplicit e
  return $ LetSimple f (ValSimpleTerm (KAbs (Unary (VN x) e'))) (KontReturn f)

transExplicit (Core.Abs (Core.Nullary e)) = do
  f <- freshV
  e' <- transExplicit e
  return $ LetSimple f (ValSimpleTerm (KAbs (Nullary e'))) (KontReturn f)

transExplicit (Core.Let (Core.ValDecl v e1) e2)  = do
  e2' <- transExplicit e2
  e1' <- transExplicit e1
  return $ LetRet (Cont (VN v) e2') e1'

transExplicit (Core.Let (Core.FunDecs decs) e2)  = do
  decs' <- transFunDecs decs
  e2' <- transExplicit e2
  return $ LetFun decs' e2'

transExplicit (Core.If e0 e1 e2)  = do
  e1' <- transExplicit e1
  e2' <- transExplicit e2
  trans e0 (\z -> return $ If z e1' e2')

-- 2018-09-28: AA; gotta double check this part of 
-- the translation
transExplicit (Core.AssertElseError e0 e1 e2 p) = do 
  e1' <- transExplicit e1   
  trans e0 (\v0 -> 
    trans e2 (\v2 -> 
      return $AssertElseError v0 e1' v2 p))


transExplicit (Core.Tuple ts)  =
  transTuple ts []
  where
    transTuple :: [Core.Term] -> [CPS.VarName] -> S KTerm
    transTuple [] acc  = do
      v <- freshV
      return $ LetSimple v (Tuple (reverse acc)) (KontReturn v)
    transTuple (t:ts) acc  =
      trans t (\v -> transTuple ts (v:acc) )

transExplicit (Core.List ts) =
  transList ts []
  where
    transList [] acc  = do
      v <- freshV
      return $ LetSimple v (List (reverse acc)) (KontReturn v)
    transList (t:ts) acc =
      trans t (\v -> transList ts (v:acc))

transExplicit (Core.ListCons h t) = do
  v <- freshV
  trans h (\h' -> trans t (\t' -> return $ LetSimple v (ListCons h' t') (KontReturn v)))

transFunDef :: Core.Lambda -> S CPS.KLambda
transFunDef (Core.Unary x e) = do

  e' <- transExplicit e
  return (CPS.Unary (VN x) e')
transFunDef (Core.Nullary e) = do
  e' <- transExplicit e
  return (CPS.Nullary e')

trans :: Core.Term -> (CPS.VarName -> S CPS.KTerm) -> S CPS.KTerm


trans (Core.Var (Core.RegVar x)) context = context (VN x)

trans (Core.Var (Core.BaseName baseName)) context = do
  x <- freshV
  kterm' <- context x
  return $ LetSimple x (Base baseName) kterm'


trans (Core.Var (Core.LibVar lib v)) context = do
  x <- freshV
  kterm' <- context x
  return $ LetSimple x (Lib lib v) kterm'
  

trans (Core.Lit i) context =
  do x <- freshV
     kterm' <- context x
     return $ LetSimple x (ValSimpleTerm (CPS.Lit i)) kterm'

trans (Core.Error e p) context = do
  x  <- freshV
  kterm <- context x
  trans e (\z -> return $ LetRet (Cont x kterm) (Error z p))

trans (Core.App e1 e2) context = do
  x  <- freshV
  kterm <- context x
  trans e1 (\z1 ->
    trans e2 (\z2 ->
      return $ LetRet (Cont x kterm) (ApplyFun z1 z2)))

trans (Core.Bin op e1 e2) context = do
  x <- freshV
  kterm <- context x
  trans e1 (\z1 ->
    trans e2 (\z2 ->
      return $ LetSimple x (CPS.Bin op z1 z2) kterm))

trans (Core.Un op e) context = do
  x <- freshV
  kterm <- context x
  trans e (\z -> return $ LetSimple x (CPS.Un op z) kterm)

trans (Core.Abs (Core.Unary x e)) context = do
  f <- freshV
  kterm <- context f
  e' <- transExplicit e
  return $ LetSimple f (ValSimpleTerm (KAbs (Unary (VN x) e'))) kterm

trans (Core.Abs (Core.Nullary e)) context = do
  f <- freshV
  kterm <- context f
  e' <- transExplicit e
  return $ LetSimple f (ValSimpleTerm (KAbs (Nullary e'))) kterm

trans (Core.Let (Core.ValDecl v e1) e2) context = do
  e2' <- trans e2 context
  e1' <- transExplicit e1
  return $ LetRet (Cont (VN v) e2') e1'

trans (Core.Let (Core.FunDecs decs) e2) context = do
  decs' <- transFunDecs decs
  e2' <- trans e2 context
  return $ LetFun decs' e2'

trans (Core.If e0 e1 e2) context = do
  v <- freshV
  kterm <- context v
  e1' <- transExplicit e1
  e2' <- transExplicit e2
  trans e0 (\z -> return $ LetRet (Cont v kterm) (If z e1' e2'))


trans (Core.AssertElseError e0 e1 e2 p) context = do 
  x <- freshV 
  kterm <- context x 
  e1' <- transExplicit e1 
  trans e0 (\z ->
    trans e2 (\z2 ->
      return $ LetRet (Cont x kterm) (AssertElseError z e1' z2 p)))
  


trans (Core.Tuple ts) context =
  transTuple ts [] context
  where
    transTuple [] acc context = do
      v <- freshV
      e' <- context v
      return $ LetSimple v (Tuple (reverse acc)) e'
    transTuple (t:ts) acc context =
      trans t (\v -> transTuple ts (v:acc) context)

trans (Core.List ts) context =
  transList ts [] context
  where
    transList [] acc context = do
      v <- freshV
      e' <- context v
      return $ LetSimple v (List (reverse acc)) e'
    transList (t:ts) acc context =
      trans t (\v -> transList ts (v:acc) context)

trans (Core.ListCons h t) context = do
  v <- freshV
  e' <- context v
  trans h (\h' -> trans t (\t' -> return $ LetSimple v (ListCons h' t') e'))


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
