{-# LANGUAGE TupleSections #-}

module Core2ANF(transProg) where 

import           Basics
import           Control.Monad.State.Lazy as State
import           qualified IRANF as ANF
import           IRANF
import qualified Core

type S = State Integer

{--

ANF transformaton; the code base is based on the Danvy & Filinski CPS 
transformation presented by A.Kennedy

--}

transFunDecs :: [Core.FunDecl] -> S [ANF.FunDef]
transFunDecs decls = do
  mapM transFunDecl decls

transFunDecl :: Core.FunDecl -> S ANF.FunDef
transFunDecl (Core.FunDecl fname (Core.Unary pat e)) = do
  e' <- transExplicit e
  return $ ANF.Fun (VN fname) (ANF.Unary (VN pat) e')

transProg :: Core.Prog -> ANF.Prog
transProg (Core.Prog imports atoms t) =
  Prog atoms $ evalState (trans t (\z -> return $Ret z)) 1

transExplicit :: Core.Term -> S ANF.ATerm
transExplicit (Core.Var (Core.RegVar x))  = return $ Ret (VN x)

transExplicit (Core.Var (Core.BaseName baseName)) = do
  x  <- freshV
  return $ Let x (Base baseName) (Ret x)

transExplicit (Core.Var (Core.LibVar lib v)) = do
  x <- freshV
  return $ Let x (Lib lib v) (Ret x)

transExplicit (Core.Lit lit) = do
  x <- freshV
  return $ Let x (ValSimpleTerm (ANF.Lit lit)) (Ret x)

transExplicit (Core.Error term) = do
  trans term (\v -> return $ Error v)

transExplicit (Core.App e1 e2) = do
  trans e1 (\x1 ->
    trans e2 (\x2 ->
      return $ TailApply x1 x2 ))

transExplicit (Core.Bin op e1 e2) = do
  x <- freshV
  trans e1 (\x1 ->
    trans e2 (\x2 ->
      return $ Let x (ANF.Bin op x1 x2) (Ret x)))

transExplicit (Core.Un op e) = do
  x <- freshV
  trans e (\x' ->
      return $ Let x (ANF.Un op x') (Ret x))

transExplicit (Core.Abs (Core.Unary x e)) = do
  f <- freshV
  e' <- transExplicit e
  return $ Let f (ValSimpleTerm (KAbs (Unary (VN x) e'))) (Ret f)

transExplicit (Core.Abs (Core.Nullary e)) = error "nullay should not be here "
  

transExplicit (Core.Let (Core.ValDecl v e1) e2)  = do
  e2' <- transExplicit e2
  e1' <- transExplicit e1
  return $ Call (VN v) e1' e2'

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
transExplicit (Core.AssertElseError e0 e1 e2) = do 
  e1' <- transExplicit e1   
  trans e0 (\v0 -> 
    trans e2 (\v2 -> 
      return $AssertElseError v0 e1' v2))


transExplicit (Core.Tuple ts)  =
  transTuple ts []
  where
    transTuple :: [Core.Term] -> [ANF.VarName] -> S ATerm
    transTuple [] acc  = do
      v <- freshV
      return $ Let v (Tuple (reverse acc)) (Ret v)
    transTuple (t:ts) acc  =
      trans t (\v -> transTuple ts (v:acc) )

transExplicit (Core.List ts) =
  transList ts []
  where
    transList [] acc  = do
      v <- freshV
      return $ Let v (List (reverse acc)) (Ret v)
    transList (t:ts) acc =
      trans t (\v -> transList ts (v:acc))

transExplicit (Core.ListCons h t) = do
  v <- freshV
  trans h (\h' -> trans t (\t' -> return $ Let v (ListCons h' t') (Ret v)))

transFunDef :: Core.Lambda -> S ANF.LambdaExp
transFunDef (Core.Unary x e) = do
  e' <- transExplicit e
  return (ANF.Unary (VN x) e')

transFunDef (Core.Nullary e) = error "should not happen"
  

trans :: Core.Term -> (ANF.VarName -> S ANF.ATerm) -> S ANF.ATerm


trans (Core.Var (Core.RegVar x)) context = context (VN x)

trans (Core.Var (Core.BaseName baseName)) context = do
  x <- freshV
  kterm' <- context x
  return $ Let x (Base baseName) kterm'


trans (Core.Var (Core.LibVar lib v)) context = do
  x <- freshV
  kterm' <- context x
  return $ Let x (Lib lib v) kterm'
  

trans (Core.Lit i) context =
  do x <- freshV
     kterm' <- context x
     return $ Let x (ValSimpleTerm (ANF.Lit i)) kterm'

trans (Core.Error e) context = do
  x  <- freshV
  kterm <- context x
  trans e (\z -> return $ Call x  (Error z) kterm)

trans (Core.App e1 e2) context = do
  x  <- freshV
  kterm <- context x
  trans e1 (\z1 ->
    trans e2 (\z2 ->
      return $ Call x (TailApply z1 z2) kterm ))

trans (Core.Bin op e1 e2) context = do
  x <- freshV
  kterm <- context x
  trans e1 (\z1 ->
    trans e2 (\z2 ->
      return $ Let x (ANF.Bin op z1 z2) kterm))

trans (Core.Un op e) context = do
  x <- freshV
  kterm <- context x
  trans e (\z -> return $ Let x (ANF.Un op z) kterm)

trans (Core.Abs (Core.Unary x e)) context = do
  f <- freshV
  kterm <- context f
  e' <- transExplicit e
  return $ Let f (ValSimpleTerm (KAbs (Unary (VN x) e'))) kterm

trans (Core.Abs (Core.Nullary e)) context = 
    error "nullaries should not be here"

trans (Core.Let (Core.ValDecl v e1) e2) context = do
  e2' <- trans e2 context
  e1' <- transExplicit e1
  return $ Call (VN v) e1' e2' 

trans (Core.Let (Core.FunDecs decs) e2) context = do
  decs' <- transFunDecs decs
  e2' <- trans e2 context
  return $ LetFun decs' e2'

trans (Core.If e0 e1 e2) context = do
  v <- freshV
  kterm <- context v
  e1' <- transExplicit e1
  e2' <- transExplicit e2
  trans e0 (\z -> return $ Call v (If z e1' e2') kterm )


trans (Core.AssertElseError e0 e1 e2) context = do 
  x <- freshV 
  kterm <- context x 
  e1' <- transExplicit e1 
  trans e0 (\z ->
    trans e2 (\z2 ->
      return $ Call x (AssertElseError z e1' z2) kterm )) 


trans (Core.Tuple ts) context =
  transTuple ts [] context
  where
    transTuple [] acc context = do
      v <- freshV
      e' <- context v
      return $ Let v (Tuple (reverse acc)) e'
    transTuple (t:ts) acc context =
      trans t (\v -> transTuple ts (v:acc) context)

trans (Core.List ts) context =
  transList ts [] context
  where
    transList [] acc context = do
      v <- freshV
      e' <- context v
      return $ Let v (List (reverse acc)) e'
    transList (t:ts) acc context =
      trans t (\v -> transList ts (v:acc) context)

trans (Core.ListCons h t) context = do
  v <- freshV
  e' <- context v
  trans h (\h' -> trans t (\t' -> return $ Let v (ListCons h' t') e'))


freshSymbol :: S String
freshSymbol = do
  n <- State.get
  put (n + 1)
  return $ ("gensym" ++ show n)

freshV :: S ANF.VarName
freshV = do
      s <- freshSymbol
      return $ VN s

