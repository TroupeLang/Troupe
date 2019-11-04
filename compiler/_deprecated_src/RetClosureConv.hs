{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}

--------------------------------------------------------------------------------
-- The closure conversion and JavaScript target code generation
--------------------------------------------------------------------------------

module RetClosureConv where

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

import Control.Monad.Identity

newtype KontName = K Basics.VarName deriving (Eq,Show,Ord,Generic)
instance Serialize KontName

data ContDef = Cont KontName VarName TTerm
    deriving (Eq)

data FunDef = Fun VarName VarName TTerm
    deriving (Eq)

data VarAccess
    = VarLocal VarName
    | VarEnv VarName
    deriving (Eq, Show)


--------------------------------------------------
-- Simple Target Term: they are much simpler now
-------------------------------------------------
data SimpleTTerm
    = Bin Basics.BinOp VarAccess VarAccess
    | Un Basics.UnaryOp VarAccess
    | Tuple [VarAccess]
    | List [VarAccess]
    | ListCons VarAccess VarAccess
    | Const C.Lit
    | BaseVar Basics.VarName
    | LibVar Basics.LibName Basics.VarName
   deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Target term. Obs: I decided to maintain the distinction between konts and
-- vars because that may be useful later in monitoring.
--------------------------------------------------------------------------------
data TTerm
    = LetVarSimple VarName SimpleTTerm TTerm
    | LetFunClosure VarName VarName VarName TTerm
    | LetEnvVar VarName [(VarName, VarAccess)] TTerm  -- setting up the closure environment
    | LetRet ContDef VarName TTerm
    | LetFun [FunDef] TTerm
    | KontReturn VarAccess
    | ApplyFun VarAccess VarAccess
    | If VarAccess TTerm TTerm
    | AssertElseError VarAccess TTerm VarAccess
    | Error VarAccess
    | Halt VarAccess
   deriving (Eq)

data TProg = Prog C.Atoms TTerm


--------------------------------------------------------------------------------
-- From CPS to internal representatino for closure conversion. In this
-- internal representation, all functions are closed but are not yet
-- hoisted: this is later done via codegeneration to IR (and
-- subsequently to JS)
--------------------------------------------------------------------------------



-- Our main function needs to be of the type

data VarLevel = VarNested Integer
                deriving (Eq, Ord, Show)

newtype KontLevel = KontNested Integer
               deriving (Eq, Ord, Show)


type Counter = Integer
type NestingLevel = Integer
type CCEnvX =  ( C.Atoms, Integer,  Map VarName VarLevel)


askLev :: CCx NestingLevel
askLev = do
  (atms, lev, vmap) <- ask
  return lev

-- boilerplate for modifying the environment

insVar :: VarName -> CCEnvX -> CCEnvX
insVar vn (atms, lev, vmap) =
    ( atms
    , lev
    , Map.insert vn (VarNested lev) vmap
    )

insVars :: [VarName] -> CCEnvX -> CCEnvX
insVars vars ccenv =
    foldl (flip insVar) ccenv vars

-- insKont :: KontName -> CCEnvX -> CCEnvX
-- insKont kn (atms, lev, kmap, vmap) =
--     ( atms
--     , lev
--     , Map.insert kn (KontNested lev) kmap
--     , vmap
--     )

incLev :: CCEnvX -> CCEnvX
incLev (atms, lev, vmap) =
    (atms, lev + 1, vmap)

type CCx = RWS CCEnvX FreesX Counter

varNameToTargetX :: VarName -> CCx VarAccess
varNameToTargetX v@(VN vname) = do
  (C.Atoms atms, lev, vmap) <- ask
  case Map.lookup v vmap of
    Just (VarNested lev') ->
        if lev' < lev
        then
            do
              tell $ [ (v, lev') ]  -- this is where we collect info about free vars
              return $ VarEnv v
        else return $ VarLocal v
    Nothing ->
        if vname `elem` atms
        then return $VarLocal v -- todo: should we declare VarAtom access intead?
        else error $ "undeclared variable: " ++ (show v)



type FreesX =  [ (VarName, Integer) ]

--------------------------------------------------

isDeclaredEarlierThan lev (_, l)  = l < lev

--------------------------------------------------
funToTargetX fname (CPS.Unary var kt) = do
  fname' <- mkClosVNameX fname
  lev <- askLev
  let filt = isDeclaredEarlierThan lev

  (tterm, frees) <-
       censor (filter filt)
          $ listen
              $ local  ((insVar var) . incLev)
                 $ cpsToTargetX kt  -- recursively translates kt to `tterm`

  return $ ( fname'
           , Fun fname' var tterm
           , freesProc frees)

-- todo: refactor these two; 2018-02-04; aa

funToTargetX fname (CPS.Nullary  kt) = do
  fname' <- mkClosVNameX fname
  lev <- askLev
  let filt = isDeclaredEarlierThan lev
  (tterm, frees) <-
    censor (filter filt) $ listen $ local   (incLev)
                 $ cpsToTargetX kt  -- recursively translates kt to `tterm`


  return $ ( fname'
           , Fun fname' (VN "$$$$dummy") tterm
           , freesProc frees)


kontToTargetX (CPS.Cont vname kt) = do
  kname' <- mkClosKNameX
  lev <- askLev
  let filt = isDeclaredEarlierThan lev
  (tterm, frees) <-
      censor (filter filt) $ listen $ local  ( (insVar vname) . incLev)
                 $ cpsToTargetX kt  -- recursively translates kt to `tterm`
  return $ (kname', Cont kname' vname tterm, freesProc frees)





--------------------------------------------------

cpsToTargetX :: CPS.KTerm -> CCx TTerm
cpsToTargetX (CPS.LetSimple vname st kt) = do
  t <- local (insVar vname)  (cpsToTargetX kt)
  case st of

    CPS.Base base -> do
      return $ LetVarSimple vname (BaseVar base) t

    CPS.Lib lib base -> do
      return $ LetVarSimple  vname (LibVar lib base) t
    
    CPS.Bin binop v1 v2 -> do
      v1' <- varNameToTargetX v1
      v2' <- varNameToTargetX v2
      return $ LetVarSimple vname (Bin binop v1' v2') t

    CPS.Un unop v -> do
      v' <- varNameToTargetX v
      return $ LetVarSimple vname (Un unop v') t

    CPS.Tuple lst -> do
      lst' <- mapM varNameToTargetX lst
      return $ LetVarSimple vname (Tuple lst') t

    CPS.List lst -> do
      lst' <- mapM varNameToTargetX lst
      return $ LetVarSimple vname (List lst') t

    CPS.ListCons v1 v2 -> do
      v1' <- varNameToTargetX v1
      v2' <- varNameToTargetX v2
      return $ LetVarSimple vname (ListCons v1' v2') t

    CPS.ValSimpleTerm (CPS.Lit lit) ->
      return $ LetVarSimple vname (Const lit) t

    CPS.ValSimpleTerm (CPS.KAbs klam) -> do
      (vname', fd, freeVars) <- funToTargetX vname klam
      env <- mkEnvX
      envVars <- splitFreeBoundX freeVars
      return $ LetFun [fd]
                  (LetEnvVar env envVars
                       (LetFunClosure (vname) env vname' t ))

cpsToTargetX (CPS.LetRet kk@(CPS.Cont _ _) kt) = do
  (kname', tcdef, freeKVs) <- kontToTargetX kk
  t <- (cpsToTargetX kt)
  envVars <- splitFreeBoundX freeKVs
  env <- mkEnvX
  return $ LetEnvVar env envVars (LetRet tcdef env t)


-- cpsToTargetX (CPS.LetRet kn kt) = do
--   k <- kontNameToTargetX kn
--   t <- cpsToTargetX kt
--   return $ LetRet k t

cpsToTargetX (CPS.LetFun fdefs kt) = do
  let vnames_orig = map  (\(CPS.Fun fname klam) -> fname) fdefs
  t <- local (insVars vnames_orig) (cpsToTargetX kt)
  (vnames', fds', frees)  <-
             unzip3 <$> mapM (\(CPS.Fun fname klam) ->
                                  local (insVars vnames_orig) (funToTargetX fname klam)) fdefs

  let freeVars = nub $ foldl (\ys xs -> xs ++ ys) [] frees
  env <- mkEnvX

  let mkT [] = t
      mkT ((v, v'):vn) = LetFunClosure v  env v' (mkT vn)

  let vnamesZ = zip vnames_orig vnames'
  lev <- askLev
  let vnames_orig' = map (\x -> (x, lev)) vnames_orig
  envVars <- splitFreeBoundX (freeVars\\vnames_orig')

  return $ LetFun fds'
                   (LetEnvVar env
                      envVars
                      (mkT vnamesZ)
                   )



cpsToTargetX (CPS.Halt v) = do
  v' <- varNameToTargetX v
  return $ Halt v'


cpsToTargetX (CPS.KontReturn v) = do
  v' <- varNameToTargetX v
  return $ KontReturn v'

cpsToTargetX (CPS.ApplyFun fname v) = do
  fname' <- varNameToTargetX fname
  v' <- varNameToTargetX v
  return $ ApplyFun fname'  v'

cpsToTargetX (CPS.If v kt1 kt2) = do
  v' <- varNameToTargetX v
  tt1 <- cpsToTargetX kt1
  tt2 <- cpsToTargetX kt2
  return $ If v' tt1 tt2

cpsToTargetX (CPS.AssertElseError v kt1 z) = do 
  v' <- varNameToTargetX v 
  z' <- varNameToTargetX z 
  tt1 <- cpsToTargetX kt1 
  return $ AssertElseError v' tt1 z'

cpsToTargetX (CPS.Error x) = do
  x' <- varNameToTargetX x
  return $ Error x'

--------------------------------------------------
-- aux functions
--------------------------------------------------

splitFreeBoundX fv = do
  (atms, lev, vmap) <- ask
  let (freeVars', boundVars) = Data.List.partition (\(_, l) -> l <= lev - 1 ) fv
  let envVars = (map (\(v,_) -> (v, VarLocal v)) boundVars)
                      ++ (map (\(v,_) -> (v, VarEnv v)) freeVars')
  return envVars

--------------------------------------------------


freesProc ff = nub ff

--------------------------------------------------
-- state accessors
--------------------------------------------------

incState :: CCx Integer
incState = do
  x <- get
  put (x + 1)
  return x

mkClosVNameX :: VarName -> CCx VarName
mkClosVNameX (VN v) = do
  x <- incState
  return $ VN ( v ++ "$clos" ++ (show x))

mkEnvX :: CCx VarName
mkEnvX = do
  x <- incState
  return $ VN (  "env$" ++ (show x))


mkClosKNameX :: CCx KontName
mkClosKNameX = do
  x <- incState
  return $ K ( "Kclos" ++ show x)


--------------------------------------------------
-- top-level
--------------------------------------------------

closureConvert :: CPS.Prog -> TProg
closureConvert (CPS.Prog (C.Atoms atms) t) =
  let atms' = C.Atoms atms
      initEnv = (atms'
                , 0  -- initial nesting counter
                , Map.empty)
      initState = 0
      (t', _) = evalRWS (cpsToTargetX t) initEnv initState
  in Prog (C.Atoms atms) t'
