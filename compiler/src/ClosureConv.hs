{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}


module ClosureConv where 

import qualified Basics
import RetCPS(VarName(..))
import qualified RetCPS as CPS
import qualified Core as C
import Control.Monad.RWS
import Data.Map.Lazy(Map)
import qualified Data.Map.Lazy as Map
import Data.Serialize(Serialize)
import GHC.Generics
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Data.List
import CompileMode

import CCIRANF as CCIR

import Control.Monad.Identity

data VarLevel = VarNested Integer
                deriving (Eq, Ord, Show)


type FreshCounter = Integer
type NestingLevel = Integer

------------------------------------------------------------
-- Type declarations
------------------------------------------------------------

------------------------------------------------------------
-- The main translation takes place in RWS monad

type CC = RWS
            CCEnv                   -- reader: the translation environment
            (FunDefs, Frees)        -- writer: hoisted funs and free variables 
            FreshCounter            -- state:  the counter for fresh name generation


type CCEnv   = (CompileMode, C.Atoms, NestingLevel, Map VarName VarLevel)
type Frees   = [(VarName, NestingLevel)]
type FunDefs = [CCIR.FunDef]



------------------------------------------------------------
-- Auxiliary functions
------------------------------------------------------------
consBB:: CCIR.IRInst -> CCIR.IRBBTree -> CCIR.IRBBTree
consBB i (BB insts t) = BB (i:insts) t

insVar :: VarName -> CCEnv -> CCEnv
insVar vn (compileMode, atms, lev, vmap) =
    ( compileMode
    , atms
    , lev
    , Map.insert vn (VarNested lev) vmap
    )

insVars :: [VarName] -> CCEnv -> CCEnv
insVars vars ccenv =
    foldl (flip insVar) ccenv vars


askLev = do
  (_, _, lev, _) <- ask
  return lev


incLev (compileMode, atms, lev, vmap) =
    (compileMode, atms, lev + 1, vmap)


-- this helper function looks up the variable name 
-- in the enviroment and checks if it should be declared as free
-- or local

transVar :: VarName -> CC VarAccess
transVar v@(VN vname) = do 
  (_, C.Atoms atms, lev, vmap) <- ask
  case Map.lookup v vmap of 
    Just (VarNested lev') -> 
      if lev' < lev 
      then do 
        tell $([], [(v, lev')]) -- collecting info about free vars 
        return $VarEnv v 
      else 
        return $ VarLocal v 
    Nothing -> 
      if vname `elem` atms
         then return $ VarLocal v 
         else error $ "undeclared variable: " ++ (show v)


transVars = mapM transVar         

isDeclaredEarlierThan lev (_, l)  = l < lev

transFunDec (VN fname) (CPS.Unary var kt) = do   
  lev <- askLev
  let filt = isDeclaredEarlierThan lev
  (bb, (_, frees)) <- censor (\(a,b) -> (id a, filter filt b))
     $ listen 
        $ local ((insVar var) . incLev)
           $ cpsToIR kt
  tell ([FunDef (HFN fname) var bb], [])
  return (nub frees)


-- state accessors

incState :: CC Integer
incState = do
  x <- get
  put (x + 1)
  return x


mkEnvBindings fv = do
  lev <- askLev
  let (freeVars', boundVars) = Data.List.partition (\(_, l) -> l <= lev - 1 ) fv
  let envVars = (map (\(v,_) -> (v, VarLocal v)) boundVars)
                      ++ (map (\(v,_) -> (v, VarEnv v)) freeVars')
  return envVars

------------------------------------------------------------
-- Main translation
------------------------------------------------------------



cpsToIR :: CPS.KTerm -> CC CCIR.IRBBTree
cpsToIR (CPS.LetSimple vname@(VN ident) st kt) = do 
    i <-
      case st of 
        CPS.Base base -> do 
          return $ CCIR.Assign vname (Base base)
        CPS.Lib lib base -> do 
          return $ CCIR.Assign vname (Lib lib base)
        CPS.Bin binop v1 v2 -> do
          v1' <- transVar v1 
          v2' <- transVar v2
          return $ CCIR.Assign vname (Bin binop v1' v2')
        CPS.Un unop v -> do 
          v' <- transVar v
          return $ CCIR.Assign vname (Un unop v')
        CPS.Tuple lst -> do 
          lst' <- transVars lst 
          return $ CCIR.Assign vname (Tuple lst')
        CPS.List lst -> do 
          lst' <- transVars lst 
          return $ CCIR.Assign vname (List lst')
        CPS.ListCons v1 v2 -> do 
          v1' <- transVar v1 
          v2' <- transVar v2 
          return $ CCIR.Assign vname (ListCons v1' v2')
        CPS.ValSimpleTerm (CPS.Lit lit) -> 
          return $ CCIR.Assign vname (Const lit)
        CPS.ValSimpleTerm (CPS.KAbs klam) -> do 
          freeVars <- transFunDec vname klam          
          envBindings <- mkEnvBindings freeVars
          return $ CCIR.MkFunClosures envBindings [(vname, HFN ident)]          
        
    t <- local (insVar vname) (cpsToIR kt)   
    return $ i `consBB` t
cpsToIR (CPS.LetRet (CPS.Cont arg kt') kt) = do
    t  <- cpsToIR kt
    t' <- local (insVar arg) (cpsToIR kt')
    return $ CCIR.BB [] $ Call arg t t'
cpsToIR (CPS.LetFun fdefs kt) = do 
    let vnames_orig = map (\(CPS.Fun fname _) -> fname) fdefs
    let localExt = local (insVars vnames_orig)
    t <- localExt (cpsToIR kt) -- translate the body

    frees <- mapM (\(CPS.Fun fname klam) -> 
                        localExt (transFunDec fname klam)) 
                fdefs

    let freeVars = (nub.concat) frees 
    lev <- askLev
    let vnames_orig' = map (\x -> (x, lev)) vnames_orig
    envBindings <- mkEnvBindings (freeVars \\ vnames_orig')
    let fnBindings = map (\x@(VN i) -> (x, HFN i)) vnames_orig
    return $ (CCIR.MkFunClosures envBindings fnBindings) `consBB` t

cpsToIR (CPS.Halt v) = do 
    v' <- transVar v
    (compileMode,_ , _ , _ ) <- ask 
    let constructor =
          case compileMode of
              Normal -> CCIR.Ret
              Export -> CCIR.LibExport

    return $ CCIR.BB [] $ constructor v'

cpsToIR (CPS.KontReturn v) = do 
  v' <- transVar v 
  return $ CCIR.BB [] $ CCIR.Ret v'

cpsToIR (CPS.ApplyFun fname v) = do 
  fname' <- transVar fname 
  v'     <- transVar v 
  return $ CCIR.BB [] $ CCIR.TailCall fname' v'

cpsToIR (CPS.If v kt1 kt2) = do 
  v' <- transVar v 
  bb1 <- cpsToIR kt1 
  bb2 <- cpsToIR kt2 
  return $ CCIR.BB [] $ CCIR.If v' bb1 bb2

cpsToIR (CPS.AssertElseError v kt1 z p) = do 
  v' <- transVar v 
  z' <- transVar z 
  bb <- cpsToIR kt1 
  return $ CCIR.BB [] $ CCIR.AssertElseError v' bb z' p

cpsToIR (CPS.Error v p) = do 
  v' <- transVar v 
  return $ CCIR.BB [] $ CCIR.Error v' p
  



------------------------------------------------------------
-- Top-level function
------------------------------------------------------------

closureConvert :: CompileMode -> CPS.Prog -> CCIR.IRProgram
closureConvert compileMode (CPS.Prog (C.Atoms atms) t) =
  let atms' = C.Atoms atms
      initEnv = ( compileMode
                , atms'
                , 0 -- initial nesting counter
                , Map.empty
                )
      initState = 0
      (bb, (fdefs, _)) = evalRWS (cpsToIR t) initEnv initState
      (argumentName, toplevel) =
         case compileMode of
           Normal -> ("$$authorityarg", "main") -- passing authority through the argument to main 
           Export -> ("$$dummy", "export")

      -- obs that our 'main' may have two names depending on the compilation mode; 2018-07-02; AA
      main = FunDef (HFN toplevel) (VN argumentName) bb

      irProg = CCIR.IRProgram (C.Atoms atms) $ fdefs++[main]
    in if CCIR.wfIRProg irProg then irProg
                               else error "the generated IR is not well-formed"

               
  
  
