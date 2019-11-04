{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}


module Core (   Lambda (..)
              , Term (..)
              , Decl (..)
              , FunDecl (..)
              , Lit(..)
              , AtomName
              , Atoms(..)
              , Prog(..)
              , VarAccess(..)
              , lowerProg
              , renameProg
              )
where
import GHC.Generics(Generic)
import Data.Serialize (Serialize)


import           Basics
import qualified DirectWOPats as D
import qualified Data.Map.Strict as Map
import           Control.Monad
import           Control.Monad.State.Lazy as State
import           Control.Monad.RWS

import qualified Text.PrettyPrint.HughesPJ as PP
import           Text.PrettyPrint.HughesPJ (
   (<+>), ($$), text, hsep, vcat, nest, nest)
import           ShowIndent

import           TroupePositionInfo

--------------------------------------------------
-- AST is the same as Direct, but lambda are unary (or nullary)

data Lambda = Unary VarName Term
            | Nullary Term
  deriving (Eq)

data Decl
    = ValDecl VarName Term
    | FunDecs [FunDecl]
  deriving (Eq )

data FunDecl = FunDecl VarName Lambda
  deriving (Eq)

data Lit
    = LInt Integer PosInf
    | LString String
    | LLabel String
    | LUnit
    | LBool Bool
    | LAtom AtomName
  deriving (Ord, Eq, Show, Generic)
instance Serialize Lit


instance GetPosInfo Lit where
  posInfo (LInt _ p) = p
  posInfo _ = NoPos

data VarAccess
    = RegVar VarName
    | LibVar LibName VarName
    | BaseName VarName
 deriving (Eq)
data Term
    = Lit Lit
    | Var VarAccess
    | Abs Lambda
    | App Term Term
    | Let Decl Term
    | If Term Term Term
    | AssertElseError Term Term Term PosInf
    | Tuple [Term]
    | List [Term]
    | ListCons Term Term
    | Bin BinOp Term Term
    | Un UnaryOp Term
    | Error Term PosInf
  deriving (Eq)


data Atoms = Atoms [AtomName]
  deriving (Eq, Show, Generic)
instance Serialize Atoms


data Prog = Prog Imports Atoms Term
  deriving (Eq, Show)


{-- 

This module defines the Core front-level intermediate representation,
and includes two phases of the compilation pipeline that involve that
representation.

1. Lowering of the program from the direct representation into the
Core representation

2. α-renaming and library name resolution. This is done in the Core
representation.

The module also contains pretty printing for the Core representation.


--}


--------------------------------------------------
-- 1. Lowering 
--------------------------------------------------

lowerProg (D.Prog imports atms term) = Prog imports (trans atms) (lower term)



-- the rest of the declarations in this part are not exported

trans :: D.Atoms -> Atoms
trans (D.Atoms atms) = Atoms atms

lowerLam (D.Lambda vs t) =
  case vs of
    [] -> Unary "$unit" (lower t)
    x:xs -> Unary x (foldr (\x b -> (Abs (Unary x b))) (lower t) xs)


lowerLit (D.LInt n pi) = LInt n pi
lowerLit (D.LString s) = LString s
lowerLit (D.LLabel s) = LLabel s
lowerLit D.LUnit = LUnit
lowerLit (D.LBool b) = LBool b
lowerLit (D.LAtom n) = LAtom n

lower :: D.Term -> Core.Term
lower (D.Lit l) = Lit (lowerLit l)
lower (D.Error t p) = Error (lower t) p
lower (D.Var v) = Var (RegVar v)
  -- 2018-07-01: AA: note that we are mapping all vars to RegVar at
  -- this stage. This is a bit of a hack. A cleaner apporach is to
  -- have a separate intermediate representation. For now we save on
  -- the engineering effort and proceed like this, because at the
  -- subsequent phase (renaming) we resolve which names are base
  -- names, which are lib names, and which are actually just regular
  -- variables.

lower (D.Abs lam) = Abs (lowerLam lam)

lower (D.App e []) = Core.App (lower e) (Lit LUnit) -- does this form even exist?
lower (D.App e es) = foldl Core.App (lower e) (map lower es)
lower (D.Let decls e) =
  foldr (\ decl t -> Let (lowerDecl decl) t) (lower e) decls
  where lowerDecl (D.ValDecl vname e) = ValDecl vname (lower e)
        lowerDecl (D.FunDecs decs) = FunDecs (map lowerFun decs)
        lowerFun  (D.FunDecl v lam) = FunDecl v (lowerLam lam)
-- lower (D.Case t patTermLst) = Case (lower t) (map (\(p,t) -> (lowerDeclPat p, lower t)) patTermLst)
lower (D.If e1 e2 e3) = If (lower e1) (lower e2) (lower e3)
lower (D.AssertElseError e1 e2 e3 p) = AssertElseError (lower e1 ) (lower e2) (lower e3) p
lower (D.Tuple terms) = Tuple (map lower terms)
lower (D.List terms) = List (map lower terms)
lower (D.ListCons t1 t2) = ListCons (lower t1) (lower t2)

-- special casing shortcutting semantics; 2018-03-06;
lower (D.Bin And e1 e2) = lower (D.If e1 e2 (D.Lit (D.LBool False)))
lower (D.Bin Or e1 e2) = lower (D.If e1 (D.Lit (D.LBool True)) e2)
lower (D.Bin op e1 e2) = Bin op (lower e1) (lower e2)
lower (D.Un op e) = Un op (lower e)


--------------------------------------------------
-- 2. α-RENAMING
--------------------------------------------------


-- This is the only function that is exported here

renameProg (Prog imports (Atoms atms) term) =
  let alist = map (\ a -> (a, a)) atms
      initEnv    = Map.fromList alist
      initReader = mapFromImports imports
      initState  = 0
      (term', _) = evalRWS (rename term initEnv) initReader initState
  in Prog imports (Atoms atms) term'

-- The rest of the declarations here are not exported

{--

The renaming occurs in RWS monad that is instantiated as follows:

* The reader is the library environment
* The state is the unique variable counter
* The output is not used so we instantiate it to a dummy unit type

Note that the environment used for tracking α-substitutions is being
threaded explicitly. That is encoded in the `Env` map.

--}


type S = RWS LibEnv () Integer

type LibEnv = Map.Map VarName LibName
type Env    = Map.Map VarName VarName


mapFromImports :: Imports -> LibEnv
mapFromImports (Imports imports) =
  foldl insLib Map.empty imports
     where
       insLib map (lib, Just defs) =
             foldl (\map def -> Map.insert def lib map) map defs
       insLib map (lib, Nothing) = error "malformed lib import data structure"
           -- TODO: 2018-07-02; better error message for the above case
           -- or even better: a data structure that avoids needing to make a check like that
           -- (we should be in theory able to do that)


unique :: VarName -> S VarName
unique v = do
  n <- State.get
  put (n + 1)
  return $ v ++ show n


lookforalpha :: VarName -> Env -> VarName
lookforalpha v m = Map.findWithDefault v v m


lookforgen :: VarName -> Env -> S VarAccess
lookforgen v m =
    case Map.lookup v m of
       Just v -> return $ RegVar v
       Nothing -> do
          libmap <- ask
          case Map.lookup v libmap of
            Just lib' -> return $ LibVar lib' v
            Nothing -> return  $ BaseName v


extend :: VarName -> VarName -> Env -> Env
extend v v' m = Map.insert v v' m

rename :: Core.Term -> Env -> S Core.Term
rename (Lit l) m = return (Lit l)
rename (Error t p) m = do 
      t' <- rename t m 
      return $ Error t' p
rename (Var (RegVar v)) m = do
  v <- lookforgen v m
  return $ Var v


rename (Var x) m  = return $ Var x
rename (Abs l) m =
  liftM Abs $ renameLambda l m
rename (App t1 t2) m = do
  t1' <- rename t1 m
  t2' <- rename t2 m
  return $ App t1' t2'
rename (Let decl t) m = do
  (m', decl') <- renameDecl decl m
  t' <- rename t m'
  return $ Let decl' t'

rename (If t1 t2 t3) m = do
  t1' <- rename t1 m
  t2' <- rename t2 m
  t3' <- rename t3 m
  return $ If t1' t2' t3'

rename (AssertElseError t1 t2 t3 p) m = do  
  t1' <- rename t1 m
  t2' <- rename t2 m
  t3' <- rename t3 m
  return $ AssertElseError t1' t2' t3' p


rename (Tuple terms) m =
  Tuple <$> mapM (flip rename m) terms
rename (List terms) m =
  List <$> mapM (flip rename m) terms
rename (ListCons t1 t2) m = do
  t1' <- rename t1 m
  t2' <- rename t2 m
  return $ ListCons  t1' t2'
rename (Bin op t1 t2) m = do
  t1' <- rename t1 m
  t2' <- rename t2 m
  return $ Bin op t1' t2'
rename (Un op e) m = do
  e' <- rename e m
  return $ Un op e'

renameLambda :: Core.Lambda -> Env -> S Core.Lambda
renameLambda (Unary v t) m = do
  v' <- unique v
  t' <- rename t $ extend v v' m
  return $ Unary v' t'
renameLambda (Nullary t) m = do
  t' <- rename t m
  return $ Nullary t'


renameDecl :: Decl -> (Map.Map VarName VarName) -> S (Map.Map VarName VarName, Decl)
renameDecl (ValDecl v t) m = do
  v' <- unique v
  let m' = extend v v' m
  t' <- rename t m
  let decl' = (ValDecl v' t')
  return (m', decl')

renameDecl (FunDecs decs) m = do
  m' <- foldM ext_funDecl m decs
  decs' <- mapM (\(FunDecl v l) -> liftM (FunDecl (lookforalpha v m')) (renameLambda l m')) decs
  let decl' = (FunDecs decs')
  return (m', decl')
  where ext_funDecl m (FunDecl v _) = do
          v' <- unique v
          return $ extend v v' m



--------------------------------------------------
-- 3. Pretty printing
--------------------------------------------------


-- show is defined via pretty printing
instance Show Term
  where show t = PP.render (ppTerm 0 t)

instance ShowIndent Prog where
  showIndent k t = PP.render (nest k (ppProg t))
--------------------------------------------------

type Precedence = Integer



ppProg :: Prog -> PP.Doc
ppProg (Prog (Imports imports) (Atoms atoms) term) =
  let ppAtoms =
        if null atoms
          then PP.empty
          else (text "datatype Atoms = ") <+>
               (hsep $ PP.punctuate (text " |") (map text atoms))

      ppImports = if null imports then PP.empty else text "<<imports>>\n"
  in ppImports $$ ppAtoms $$ ppTerm 0 term


ppTerm :: Precedence -> Term -> PP.Doc
ppTerm parentPrec t =
   let thisTermPrec = termPrec t
   in PP.maybeParens (thisTermPrec < parentPrec )
      $ ppTerm' t

   -- uncomment to pretty print explicitly; 2017-10-14: AA
   -- in PP.maybeParens (thisTermPrec < 10000)  $ ppTerm' t

ppTerm' :: Term -> PP.Doc
ppTerm' (Lit literal) = ppLit literal

ppTerm' (Error t _) = text "error " PP.<> ppTerm' t

ppTerm'  (Tuple ts) =
  PP.parens $
  PP.hcat $
  PP.punctuate (text ",") (map (ppTerm 0) ts)

ppTerm'  (List ts) =
  PP.brackets $
  PP.hcat $
  PP.punctuate (text ",") (map (ppTerm 0) ts)



ppTerm' (ListCons hd tl) =
   ppTerm consPrec hd PP.<> text "::" PP.<> ppTerm consPrec tl

ppTerm' (Var (RegVar x)) = text x
ppTerm' (Var (LibVar (LibName lib) var)) = text lib <+> text "." <+> text var
ppTerm' (Var (BaseName v)) = text v
ppTerm' (Abs lam) =
  let (ppArgs, ppBody) = qqLambda lam
  in text "fn" <+> ppArgs <+> text "=>" <+> ppBody

ppTerm' (App t1 t2s) =
    ppTerm appPrec t1
          <+> (ppTerm argPrec t2s)

ppTerm' (Let dec body) =
  text "let" <+>
  nest 3 (ppDecl dec) $$
  text "in" <+>
  nest 3 (ppTerm 0 body) $$
  text "end"


ppTerm' (If e0 e1 e2) =
  text "if" <+>
  ppTerm 0 e0 $$
  text "then" <+>
  ppTerm 0 e1 $$
  text "else" <+>
  ppTerm 0 e2

ppTerm' (AssertElseError e0 e1 e2 _) =
  text "assert" <+>
  ppTerm 0 e0 $$
  text "then" <+>
  ppTerm 0 e1 $$
  text "elseError" <+>
  ppTerm 0 e2



ppTerm' (Bin op t1 t2) =
  let binOpPrec = opPrec op
  in
     ppTerm binOpPrec t1 <+>
     text (show op) <+>
     ppTerm binOpPrec t2

ppTerm' (Un op t) =
  let unOpPrec = op1Prec op
  in
     text (show op) <+>
     ppTerm unOpPrec t

qqLambda :: Lambda -> (PP.Doc, PP.Doc)
qqLambda (Unary arg body) =
  ( text arg, ppTerm 0 body )
qqLambda (Nullary body) =
  ( text "()", ppTerm 0 body)

ppDecl :: Decl -> PP.Doc
ppDecl (ValDecl arg t) =
  text "val" <+> text arg <+> text "="
    <+> ppTerm 0 t
ppDecl (FunDecs fs) = ppFuns fs
  where
    ppFunDecl prefix (FunDecl fname lam) =
      ppFunOptions (prefix ++ " " ++ fname) lam

    ppFunOptions prefix lam =
        let (ppArgs, ppBody) = qqLambda lam in
        text prefix <+> ppArgs <+> text "=" <+> nest 2 ppBody


    ppFuns (doc:docs) =
      let ppFirstFun = ppFunDecl "fun"
          ppOtherFun = ppFunDecl "and"
      in ppFirstFun doc $$ vcat (map ppOtherFun docs)


    ppFuns _ = PP.empty


ppLit :: Lit -> PP.Doc
ppLit (LInt i _)      = PP.integer i
ppLit (LString s)   = PP.doubleQuotes (text s)
ppLit (LLabel s)    = PP.braces (text s)
ppLit LUnit         = text "()"
ppLit (LBool True)  = text "true"
ppLit (LBool False) = text "false"
ppLit (LAtom a) = text a

opPrec :: BinOp -> Precedence
opPrec Plus  = 100
opPrec Minus = 100
opPrec Mult  = 200
opPrec Div   = 200
opPrec Eq    = 50
opPrec Neq   = 50
opPrec Le    = 50
opPrec Lt    = 50
opPrec Ge    = 50
opPrec Gt    = 50
opPrec And   = 50
opPrec Index = 50
opPrec FlowsTo    = 50
opPrec RaisedTo   = 50

op1Prec :: UnaryOp -> Precedence
op1Prec x = 50

appPrec :: Precedence
appPrec = 5000

argPrec :: Precedence
argPrec = appPrec + 1

maxPrec :: Precedence
maxPrec = 100000

consPrec :: Precedence
consPrec = 6000

termPrec :: Term -> Precedence
termPrec (Lit _)         = maxPrec
termPrec (Tuple _)       = maxPrec
termPrec (List _ )       = maxPrec
termPrec (Var _)         = maxPrec
termPrec (App _ _)       = appPrec
termPrec (Bin op _ _)    = opPrec op
termPrec (ListCons _ _)  = 200
termPrec _               = 0
