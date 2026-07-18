{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}


module ClosureConv where 

import InternalError (internalError)
import RetCPS(VarName(..))
import qualified RetCPS as CPS
import qualified Core as C
import Control.Monad.RWS
import Data.Map.Lazy(Map)
import qualified Data.Map.Lazy as Map
import Data.List
import CompileMode

import           Control.Monad.Except
import IR as CCIR

import TroupePositionInfo (Located(..), PosInf(..))

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
            CCEnv                         -- reader: the translation environment
            (FunDefs, Frees, ConstTracking)      -- writer: hoisted funs and free variables 
            FreshCounter                  -- state:  the counter for fresh name generation


type CCEnv   = (CompileMode, NestingLevel, Map VarName VarLevel, Maybe VarName)
type Frees   = [(VarName, NestingLevel)]
type FunDefs = [CCIR.LFunDef]
type ConstEntry = (VarName, C.Lit)
type ConstTracking = [(ConstEntry, NestingLevel)]


------------------------------------------------------------
-- Auxiliary functions
------------------------------------------------------------
consBB:: CCIR.LIRInst -> CCIR.IRBBTree -> CCIR.IRBBTree
consBB i (BB insts t) = BB (i:insts) t

insVar :: VarName -> CCEnv -> CCEnv
insVar vn (compileMode, lev, vmap, fname) =
    ( compileMode
    , lev
    , Map.insert vn (VarNested lev) vmap
    , fname
    )

insVars :: [VarName] -> CCEnv -> CCEnv
insVars vars ccenv =
    foldl (flip insVar) ccenv vars


askLev = do
  (_, lev, _, _) <- ask
  return lev


incLev fname (compileMode, lev, vmap, _) =
    (compileMode, lev + 1, vmap, (Just fname))


-- this helper function looks up the variable name
-- in the enviroment and checks if it should be declared as free
-- or local

transVar :: VarName -> CC VarAccess
transVar v@(VN vname) = do
  (_, lev, vmap, maybe_fname) <- ask
  case maybe_fname of
    Just fname | fname == v  -> return $ VarFunSelfRef
    _ ->
      case Map.lookup v vmap of
        Just (VarNested lev') ->
          if lev' < lev
          then do
            tell $ ([], [(v, lev')], []) -- collecting info about free vars
            return $ VarEnv v
          else
            return $ VarLocal v
        Nothing ->
          internalError $ "undeclared variable: " ++ (show v)

-- | Translate a Located VarName (LVarName) to Located VarAccess (LVarAccess)
-- Preserves the source position from the input
transLVar :: CPS.LVarName -> CC CCIR.LVarAccess
transLVar (Loc pos vn) = do
  va <- transVar vn
  return $ Loc pos va

transVars :: [CPS.VarName] -> CC [CCIR.VarAccess]
transVars = mapM transVar

-- | Translate a list of Located VarNames to Located VarAccesses
transLVars :: [CPS.LVarName] -> CC [CCIR.LVarAccess]
transLVars = mapM transLVar         

isDeclaredEarlierThan lev (_, l)  = l < lev

-- Translate function declaration to LFunDef (Located FunDef)
-- The function definition position goes on the Located wrapper
transFunDec f@(VN fname) (CPS.Unary (Loc varPos var) lkt) pos = do
  lev <- askLev
  let filt = isDeclaredEarlierThan lev
  (bb, (_, frees, consts_wo_levs)) <-
      censor (\(a,b,c ) -> (a, filter filt b, filter (\(_, l) -> l == lev ) c))
     $ listen
        $ local ((insVar var) . (incLev f))
           $ cpsToIR lkt
  let consts = (fst.unzip) consts_wo_levs
  -- Wrap FunDef with Located, using pos for function definition position
  tell ([Loc pos (FunDef (HFN fname) (Loc varPos var) consts bb)], [], [])
  return (nub frees)

transFunDec (VN _) (CPS.Nullary _) _ = internalError "transFunDec: not implemented for nullary functions"

-- state accessors

incState :: CC Integer
incState = do
  x <- get
  put (x + 1)
  return x


mkEnvBindings :: PosInf -> Frees -> CC [(VarName, LVarAccess)]
mkEnvBindings pos fv = do
  lev <- askLev
  let (freeVars', boundVars) = Data.List.partition (\(_, l) -> l <= lev - 1 ) fv
  let envVars = (map (\(v,_) -> (v, Loc pos (VarLocal v))) boundVars)
                      ++ (map (\(v,_) -> (v, Loc pos (VarEnv v))) freeVars')
  return envVars

------------------------------------------------------------
-- Main translation
------------------------------------------------------------

-- | Translate CPS LFields (with LVarName) to IR LFields (with LVarAccess)
transLFields :: CPS.LFields -> CC CCIR.LFields
transLFields fields = do
          let (ff, lvv) = unzip fields
          lst' <- transLVars lvv
          return $ zip ff lst'

-- | cpsToIR translates CPS terms to IR, producing proper Located IR types.
-- Positions from CPS Located wrappers are used to wrap IR constructs.
cpsToIR :: CPS.LKTerm -> CC CCIR.IRBBTree
cpsToIR (Loc pos (CPS.LetSimple vname@(VN ident) (Loc stPos st) lkt)) = do
    i <-
      -- Helper to create Located Assign instruction
      let _assign arg = return $ Just $ Loc stPos (CCIR.Assign vname arg) in
      case st of
        CPS.Base base -> _assign $ Base base
        CPS.Lib lib base -> _assign (Lib lib base)
        -- Now using transLVar to translate LVarName to LVarAccess
        CPS.Bin binop lv1 lv2 -> do
          lv1' <- transLVar lv1
          lv2' <- transLVar lv2
          return $ Just $ Loc stPos $ CCIR.Assign vname (Bin binop lv1' lv2')
        CPS.Un unop lv -> do
          lv' <- transLVar lv
          return $ Just $ Loc stPos $ CCIR.Assign vname (Un unop lv')
        CPS.Tuple lst tag -> do
          lst' <- transLVars lst
          return $ Just $ Loc stPos $ CCIR.Assign vname (Tuple lst' tag)
        CPS.Record fields -> do
          fields' <- transLFields fields
          return $ Just $ Loc stPos $ CCIR.Assign vname (Record fields')
        CPS.WithRecord lv fields -> do
          lv' <- transLVar lv
          fields' <- transLFields fields
          return $ Just $ Loc stPos $ CCIR.Assign vname (WithRecord lv' fields')
        CPS.ProjField lv f -> do
          lv' <- transLVar lv
          return $ Just $ Loc stPos $ CCIR.Assign vname (ProjField lv' f)
        CPS.ProjIdx lv idx -> do
          lv' <- transLVar lv
          return $ Just $ Loc stPos $ CCIR.Assign vname (ProjIdx lv' idx)
        CPS.List lst -> do
          lst' <- transLVars lst
          return $ Just $ Loc stPos $ CCIR.Assign vname (List lst')
        CPS.ListCons lv1 lv2 -> do
          lv1' <- transLVar lv1
          lv2' <- transLVar lv2
          return $ Just $ Loc stPos $ CCIR.Assign vname (ListCons lv1' lv2')
        CPS.ValSimpleTerm (CPS.Lit lit) -> do lev <- askLev
                                              tell ([],[],[((vname, lit), lev)])
                                              return Nothing
        CPS.ValSimpleTerm (CPS.KAbs klam) -> do
          freeVars <- transFunDec vname klam stPos
          envBindings <- mkEnvBindings stPos freeVars
          return $ Just $ Loc stPos $ CCIR.MkFunClosures envBindings [(vname, HFN ident)]

    t <- local (insVar vname) (cpsToIR lkt)
    return $ case i of
      Just i' -> i' `consBB` t
      Nothing -> t

cpsToIR (Loc pos (CPS.LetRet (CPS.Cont arg lkt') lkt)) = do
    t  <- cpsToIR lkt
    t' <- local (insVar arg) (cpsToIR lkt')
    return $ CCIR.BB [] $ Loc pos $ StackExpand arg t t'

cpsToIR (Loc _pos (CPS.LetFun lfdefs lkt)) = do
    let vnames_orig = map (\(Loc _ (CPS.Fun fname _)) -> fname) lfdefs
    let localExt = local (insVars vnames_orig)
    t <- localExt (cpsToIR lkt) -- translate the body

    frees <- mapM (\(Loc funPos (CPS.Fun fname klam)) ->
                        localExt (transFunDec fname klam funPos))
                lfdefs

    let freeVars = (nub.concat) frees
    lev <- askLev
    let vnames_orig' = map (\x -> (x, lev)) vnames_orig
    -- Use the position of the first function definition for the closure instruction
    let funDeclPos = case lfdefs of
          (Loc p _ : _) -> p
          [] -> NoPos
    envBindings <- mkEnvBindings funDeclPos (freeVars \\ vnames_orig')
    let fnBindings = map (\x@(VN i) -> (x, HFN i)) vnames_orig
    return $ (Loc funDeclPos $ CCIR.MkFunClosures envBindings fnBindings) `consBB` t

-- Special Halt continuation, for exiting program
-- Note: Halt takes a plain VarName, so we use pos for the LVarAccess wrapper
cpsToIR (Loc pos (CPS.Halt v)) = do
    v' <- transVar v
    let lv' = Loc pos v'  -- Wrap VarAccess with position to create LVarAccess
    (compileMode, _, _, _) <- ask
    let terminator =
          case compileMode of
              -- Compiling library, then generate export instruction
              CompileMode.Library -> Loc pos $ CCIR.LibExport lv'
              -- Otherwise, keep it as a simple return
              _                   -> Loc pos $ CCIR.Ret lv'

    return $ CCIR.BB [] terminator

-- Note: KontReturn takes a plain VarName, so we use pos for the LVarAccess wrapper
cpsToIR (Loc pos (CPS.KontReturn v)) = do
  v' <- transVar v
  let lv' = Loc pos v'  -- Wrap VarAccess with position to create LVarAccess
  return $ CCIR.BB [] $ Loc pos $ CCIR.Ret lv'

-- Note: ApplyFun takes plain VarNames, so we use pos for the LVarAccess wrappers
cpsToIR (Loc pos (CPS.ApplyFun fname v)) = do
  fname' <- transVar fname
  v'     <- transVar v
  let lfname' = Loc pos fname'  -- Wrap VarAccess with position
  let lv' = Loc pos v'          -- Wrap VarAccess with position
  return $ CCIR.BB [] $ Loc pos $ CCIR.TailCall lfname' lv'

-- Note: If takes a plain VarName, so we use pos for the LVarAccess wrapper
cpsToIR (Loc pos (CPS.If v lkt1 lkt2)) = do
  v' <- transVar v
  let lv' = Loc pos v'  -- Wrap VarAccess with position
  bb1 <- cpsToIR lkt1
  bb2 <- cpsToIR lkt2
  return $ CCIR.BB [] $ Loc pos $ CCIR.If lv' bb1 bb2

-- AssertElseError and Error: position comes from Located wrapper
-- Note: AssertElseError takes plain VarNames, so we use pos for the LVarAccess wrappers
cpsToIR (Loc pos (CPS.AssertElseError v lkt1 z)) = do
  v' <- transVar v
  z' <- transVar z
  let lv' = Loc pos v'  -- Wrap VarAccess with position
  let lz' = Loc pos z'  -- Wrap VarAccess with position
  bb <- cpsToIR lkt1
  return $ CCIR.BB [] $ Loc pos $ CCIR.AssertElseError lv' bb lz'

-- Note: Error takes a plain VarName, so we use pos for the LVarAccess wrapper
cpsToIR (Loc pos (CPS.Error v)) = do
  v' <- transVar v
  let lv' = Loc pos v'  -- Wrap VarAccess with position
  return $ CCIR.BB [] $ Loc pos $ CCIR.Error lv'
  



------------------------------------------------------------
-- Top-level function
------------------------------------------------------------

closureConvert :: CompileMode -> CPS.Prog -> Except String CCIR.IRProgram
closureConvert compileMode (CPS.Prog lkt) =
  let initEnv = ( compileMode
                , 0 -- initial nesting counter
                , Map.empty
                , Nothing -- top level code has no function name
                )
      initState = 0
      (bb, (fdefs, _, consts_wo_levs)) = evalRWS (cpsToIR lkt) initEnv initState
      (argumentName, toplevel) =
         case compileMode of
           -- Top level function of a library is named 'export'
           CompileMode.Library     -> ("$$dummy", "export")
           -- Passing authority through the argument to main
           _                       -> ("$$authorityarg", "main")


      -- obs that our 'main' may have two names depending on the compilation mode; 2018-07-02; AA
      consts = (fst.unzip) consts_wo_levs
      -- The main entry point is compiler-generated, so it has no source position
      -- Wrap FunDef with Located (NoPos since it's compiler-generated)
      main = Loc NoPos $ FunDef (HFN toplevel) (Loc NoPos (VN argumentName)) consts bb

      irProg = CCIR.IRProgram $ fdefs++[main]
    in do CCIR.wfIRProg irProg
          return irProg
    -- then irProg
    --                       else error "the generated IR is not well-formed"

               
  
  
