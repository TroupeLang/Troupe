{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module RawOpt (rawopt) where
import Raw
import qualified Data.Maybe
import Control.Monad.RWS.Lazy
import Control.Monad
import Data.Map.Lazy (Map)
import Data.Set(Set)
import qualified Data.List 
import RawDefUse (iDefUse)
import qualified Data.Set as Set 
import qualified Basics
import qualified Core
import           RetCPS (VarName (..))
import qualified Data.Map.Lazy as Map 
import           IR ( Identifier(..)
                    , VarAccess(..), HFN (..), Fields (..), Ident
                    , ppId,ppFunCall,ppArgs
                    )
import qualified Data.List
import qualified Data.Ord

--------------------------------------------------
--  substitutions for Raw 
--------------------------------------------------
newtype Subst = Subst (Map RawVar RawVar)

class Substitutable a where
  apply :: Subst -> a -> a

idSubst :: Subst
idSubst = Subst (Map.empty)




instance Substitutable RawVar where 
    apply subst@(Subst varmap) x =
        Map.findWithDefault x x varmap

instance Substitutable RawExpr where 
  apply subst e = 
    case e of 
      Bin op x y -> Bin op (apply subst x) (apply subst y)
      Un op x -> Un op (apply subst x)
      ListCons x y -> ListCons x (apply subst y)
      WithRecord x fs -> WithRecord (apply subst x) fs 
      ProjField x f -> ProjField (apply subst x) f
      ProjIdx x n -> ProjIdx (apply subst x) n
      ConstructLVal r1 r2 r3 -> 
        ConstructLVal (apply subst r1) (apply subst r2) (apply subst r3)
      _ -> e 

-- | Defining how to apply a substitution.
instance Substitutable RawInst where 
  apply subst i = 
    case i of 
      AssignRaw r1 r2 -> AssignRaw (apply subst r1) (apply subst r2)
      SetState mc r -> SetState mc (apply subst r)
      AssignLVal v e -> AssignLVal v (apply subst e)
      RTAssertion a -> RTAssertion $ case a of
        AssertType r t -> AssertType (apply subst r) t
        -- AssertEqTypes ts r1 r2 -> AssertEqTypes ts (apply subst r1) (apply subst r2)
        AssertTypesBothStringsOrBothNumbers r1 r2 -> AssertTypesBothStringsOrBothNumbers (apply subst r1) (apply subst r2)
        AssertTupleLengthGreaterThan v n -> AssertTupleLengthGreaterThan (apply subst v) n
        AssertRecordHasField v f -> AssertRecordHasField (apply subst v) f
      InvalidateSparseBit -> i
      _ -> i

instance Substitutable RawTerminator where 
  apply subst tr = 
    case tr of 
      TailCall r -> TailCall (apply subst r) 
      If r bb1 bb2 -> 
        If (apply subst r) (apply subst bb1) (apply subst bb2)
      Error r p -> Error (apply subst r) p 
      Call bb1 bb2 -> Call (apply subst bb1) (apply subst bb2) 
      _ -> tr

instance Substitutable RawBBTree where
  apply subst (BB ins tr) = 
    BB (map (apply subst) ins) (apply subst tr)


-- end of substitutions 

-- | Stores inferred information from the traversal.
data PState = 
    PState { stateMon   :: Map MonComponent RawVar,               -- monitor state 
             stateLVals :: Map (VarName, LValField) RawVar,       -- lvalues 
             stateJoins :: Map (RawVar, RawVar) RawVar,           -- computed joins
             stateSubst :: Subst,
             stateChange:: ChangeFlag,
             stateTypes :: Map RawVar RawType                     -- for assertions optimizations
           }

-- 2021-02-28; AA 
-- As we traverse the AST we collect information about how 
-- different bindings are used. We distinguish two different 
-- used collections:
-- 
-- 1) used lvals,
-- 2) used rawvars, 

-- 
type Used = (Set VarName, Set RawVar) 

type ChangeFlag = Bool

-- Optimization monad: keep track of used variables, to be able to eliminate unused variables.
type Opt = RWS () Used PState

class PEval a where 
    peval :: a -> Opt a 

class MarkUsed a where 
  markUsed :: a -> Opt ()

instance MarkUsed VarName where 
  markUsed vn = tell (Set.singleton vn, Set.empty)

instance MarkUsed RawVar where 
  markUsed rv = tell (Set.empty, Set.singleton rv)


instance MarkUsed VarAccess where 
  markUsed (VarLocal vn) = markUsed vn 
  markUsed _ = return ()

instance MarkUsed a => MarkUsed [a] where 
  markUsed ls = mapM_ markUsed ls

instance MarkUsed RawExpr where 
  markUsed e = case e of 
    Bin _ x y -> markUsed [x,y]
    Un _ x -> markUsed x
    ProjectLVal x _ -> markUsed x
    ProjectState _ -> return ()
    Tuple xs -> markUsed xs 
    Record fields -> markUsed (snd (unzip fields))
    WithRecord x fields -> do 
      markUsed x 
      markUsed (snd (unzip fields))
    ProjField x _ -> markUsed x
    ProjIdx x _ -> markUsed x
    List xs -> markUsed xs
    ListCons x y -> markUsed x >> markUsed y 
    Const _ -> return ()
    Lib _ _ -> return ()
    Base _ -> return ()    
    ConstructLVal x y z -> markUsed [x,y,z]


-- | Apply current substitution of RawVar to other RawVar.
-- use to keep track of which vars can be subst for each other, e.g. after "x=a"
subst x = do 
  s <- get 
  return $ apply (stateSubst s) x

-- | Remember that have to replace x with y.
addSubst x y = do 
  s <- get 
  let (Subst m) = stateSubst s 
  put $ s { stateSubst = Subst (Map.insert x y m)}

-- | Remember that pc/block (first argument) can be found in variable r (second argument).
monInsert p r = do 
  s <- get 
  let mon = Map.insert p r (stateMon s) 
  put $ s {stateMon = mon}

monLookup x s = Map.lookup x (stateMon s)

typeOfLit :: Core.Lit -> Maybe RawType
typeOfLit lit = 
    case lit of 
      Core.LUnit -> Just RawUnit
      Core.LInt _ _ -> Just RawNumber 
      Core.LString _ -> Just RawString
      Core.LLabel _ -> Just RawLevel               
      Core.LBool _ -> Just RawBoolean 
      Core.LAtom _ -> Nothing

guessType :: RawExpr -> Maybe RawType
guessType = \case
  Const lit -> typeOfLit lit

  Bin op _ _ -> case op of
    Basics.Plus -> Just RawNumber
    Basics.Minus -> Just RawNumber
    Basics.Div -> Just RawNumber
    Basics.Mult -> Just RawNumber
    Basics.Mod -> Just RawNumber
    Basics.BinAnd -> Just RawNumber
    Basics.BinXor -> Just RawNumber
    Basics.BinShiftLeft -> Just RawNumber
    Basics.BinShiftRight -> Just RawNumber
    Basics.BinZeroShiftRight -> Just RawNumber
    Basics.Eq -> Just RawBoolean
    Basics.Neq -> Just RawBoolean
    Basics.Le -> Just RawBoolean
    Basics.Lt -> Just RawBoolean
    Basics.Ge -> Just RawBoolean
    Basics.Gt -> Just RawBoolean
    Basics.And -> Just RawBoolean
    Basics.Or -> Just RawBoolean
    Basics.HasField -> Just RawBoolean
    Basics.Concat -> Just RawString
    -- Revision 2023-08: Added missing cases
    Basics.IntDiv -> Just RawNumber
    Basics.BinOr -> Just RawNumber
    Basics.FlowsTo -> Just RawBoolean
    Basics.LatticeJoin -> Just RawLevel
    Basics.LatticeMeet -> Just RawLevel
    Basics.RaisedTo -> Nothing -- depends on operand type

  Un op x -> case op of
    Basics.ListLength -> Just RawNumber
    Basics.TupleLength -> Just RawNumber
    Basics.UnMinus -> Just RawNumber
    Basics.IsTuple -> Just RawBoolean
    Basics.IsList -> Just RawBoolean
    Basics.IsRecord -> Just RawBoolean
    -- Revision 2023-08: Added missing cases
    Basics.Fst -> Nothing
    Basics.Snd -> Nothing
    Basics.Head -> Nothing
    Basics.Tail -> Nothing
    Basics.LevelOf -> Just RawLevel

  Tuple _ -> Just RawTuple
  List _ -> Just RawList
  ListCons _ _ -> Just RawList
  Record _ -> Just RawRecord
  WithRecord _ _  -> Just RawRecord
  -- Revision 2023-08: Added missing cases
  ProjField _ _ -> Nothing
  ProjIdx _ _ -> Nothing
  ProjectLVal _ FieldValLev -> Just RawLevel
  ProjectLVal _ FieldTypLev -> Just RawLevel
  ProjectLVal _ FieldValue -> Nothing
  ProjectState MonPC -> Just RawLevel
  ProjectState MonBlock -> Just RawLevel
  ProjectState R0_Lev -> Just RawLevel
  ProjectState R0_TLev -> Just RawLevel
  ProjectState R0_Val -> Nothing
  Lib _ _ -> Nothing
  Base _ -> Nothing
  ConstructLVal _ _ _ -> Nothing

_setType x t = modify (\pstate -> 
          pstate { stateTypes = Map.insert x t (stateTypes pstate)})

-- Partially evaluate instruction. This is called multiple times in the optimization sequence.
-- First pass: partially evaluate functions (instructions).
-- Removes e.g. redundant state projections state (e.g. if multiple in same block).
-- Returns 'Nothing' if the instruction is to be ommitted.
pevalInst:: RawInst -> Opt (Maybe RawInst)
pevalInst i = do 
    pstate <- get
    i' <- subst i -- apply the collected substitutions
    let _omit x = x >> (return Nothing)
    let _keep x = x >> (return $ Just i')
             
    case i' of 
      AssignRaw r (ProjectState p) -> do
        case monLookup p pstate of -- lookup the known state of the monitor component
          Just r' -> _omit $ addSubst r r' -- The state can already be found in r', therefore the assignment to r can be omitted, and we have to remember to substitute r with r'.
          Nothing -> _keep $ monInsert p r -- remember that PC/block can be found in variable r
      AssignRaw r (Bin Basics.LatticeJoin x y) -> do 
        if x == y then _omit (addSubst r x) -- trivial join
        else do
          case Map.lookup (x,y) (stateJoins pstate) of 
            Just r' -> _omit $ addSubst r r'
            Nothing -> case Map.lookup (x,y) (stateJoins pstate) of
              Just r' -> _omit $ addSubst r r'
              Nothing -> _keep $ do 
                markUsed [x,y] 
                put $ pstate { stateJoins = Map.insert (x,y) r (stateJoins pstate) }
              
      AssignLVal v (ConstructLVal r1 r2 r3) -> _keep $ do
        markUsed [r1, r2, r3]
        
        
        let m0 = stateLVals pstate
        let m1 = Map.insert (v, FieldValue) r1 m0
        let m2 = Map.insert (v, FieldValLev) r2 m1 
        let m3 = Map.insert (v, FieldTypLev) r3 m2        
        put $ pstate { stateLVals = m3 }        
      AssignRaw r (ProjectLVal (VarLocal v) field) -> do        
        case (Map.lookup (v, field) (stateLVals pstate)) of           
          Just r' -> _omit $ addSubst r r'             
          Nothing -> _keep $ do 
            markUsed v
            let m0 = stateLVals pstate 
            let m1 = Map.insert (v, field) r m0 
            put $ pstate { stateLVals = m1 }            
      AssignRaw r rexpr -> _keep $ do
        markUsed rexpr 
        case guessType rexpr of 
            Nothing -> return ()
            Just ty -> _setType r ty 
        

      AssignLVal v complexExpr -> _keep $ markUsed complexExpr
      SetState p r -> _keep $ do 
        markUsed r
        monInsert p r        
      RTAssertion (AssertType r rt) -> do 
        case Map.lookup r (stateTypes pstate) of 
          Just rt' | rt' == rt -> return Nothing
          _ -> _keep $ _setType r rt >> markUsed r        
      -- RTAssertion (AssertEqTypes opt_ls x y) -> do
      --   let _m = stateTypes pstate
      --   let keep = _keep $ markUsed [x,y]
      --   case (Map.lookup x _m, Map.lookup y _m) of
      --     (Just t1 , Just t2) | t1 == t2 ->
      --       case opt_ls of
      --         Nothing -> return Nothing
      --         Just (List2OrMore p1 p2 ps) ->
      --           if t1 `elem` (p1:p2:ps) then
      --             return Nothing
      --           else keep
      --     _ -> keep
      RTAssertion (AssertTypesBothStringsOrBothNumbers x y) -> do 
        let _m = stateTypes pstate 
        let keep = _keep $ markUsed [x,y]
        case (Map.lookup x _m, Map.lookup y _m) of 
          (Just t1 , Just t2) | t1 == t2 -> 
            if t1 `elem` [RawNumber, RawString]
            then return Nothing
            else keep
          _ -> keep
      -- TODO track tuple length
      RTAssertion (AssertTupleLengthGreaterThan r n) -> _keep $ markUsed r
      -- TODO track record fields
      RTAssertion (AssertRecordHasField r f) -> _keep $ markUsed r
      MkFunClosures ee _ -> _keep $ markUsed (snd (unzip ee)) 
      -- No applicable optimizations.
      SetBranchFlag -> return $ Just i'
      InvalidateSparseBit -> return $ Just i'
                          

instance PEval RawTerminator where
  peval tr = do 
    tr' <- subst tr -- todo: obs complexity :( 2021-02-23; AA
    case tr' of 
      If x bb1 bb2 -> do
        markUsed x
        s <- get
        bb1' <- peval bb1
        -- undo stateful effects before switching to another branch
        put $ s { stateMon = stateMon s
                , stateLVals = stateLVals s
                , stateJoins = stateJoins s
                }
        bb2' <- peval bb2         
        return $ If x bb1' bb2'
      Call bb1 bb2 -> do
        s <- get
        bb1' <- peval bb1
        put $ s { stateMon = Map.empty
                , stateLVals = stateLVals s
                , stateJoins = stateJoins s 
                } -- reset the monitor state
        bb2' <- peval bb2
        return $ Call bb1' bb2'
      Ret -> do 
        return tr' 
      TailCall x -> do 
        markUsed x
        return tr' 
      Error x _ -> do 
        markUsed x 
        return tr'
      LibExport x -> do
        markUsed x 
        return tr'
      
  
isLiveInstFwd :: Used -> RawInst -> Bool
isLiveInstFwd (lvals, rvars) i = 
  case i of
    AssignRaw r _ -> Set.member r rvars 
    AssignLVal v _ -> Set.member v lvals 
    _ -> True


filterInstBwd :: [RawInst] -> ([RawInst], [RawInst])
filterInstBwd ls = 
  let f (pc, bl) (i:is) acc = 
        case i of 
          SetState MonPC _ -> 
            if pc /= Nothing
                  then f (pc, bl) is acc
                  else f (Just i, bl) is acc
          SetState MonBlock _ -> 
            if bl /= Nothing 
                  then f (pc, bl) is acc 
                  else f (pc, Just i) is acc
          _ -> f (pc, bl) is (i:acc)
      f (pc, bl) [] acc = 
        let fromJ (Just x) = [x]
            fromJ Nothing = []
         in (acc, concat $ map fromJ [pc, bl]) in 
  f (Nothing, Nothing) (reverse ls) []


-- | This optimization for 'Call' moves instructions from the continuation to before the 'Call'.
-- This can result in a 'Call' which just contains a 'Ret', which is then optimized away.
-- The optimization compensates for redundant assignments introduced by the translation.
hoistCalls :: RawBBTree -> RawBBTree
hoistCalls bb@(BB insts tr) = 
  case tr of 
    -- Here we check which instructions from ii_1 can be moved to before the call
    Call (BB ii_1 tr_1) bb2 ->
      let isFrameSpecific i = 
            case i of 
              SetBranchFlag -> True
              SetState _ _ -> True 
              InvalidateSparseBit -> True -- to be safe, we define this frame-specific
              _ -> False
          -- jx_1: non-frame-specific instructions, are moved to before the call
          -- jx_2: frame-specific instructions, stay under the call's instructions
          (jx_1, jx_2)  = Data.List.break isFrameSpecific ii_1  
      in BB (insts ++ jx_1) (Call (BB jx_2 tr_1) bb2) 
    -- If returning, the current frame will be removed, and thus all PC set instructions
    -- are redundant and can be removed.
    Ret -> 
      let isNotPcSet (SetState MonPC _) = False
          isNotPcSet _ = True 
          insts_wo_PCUpd = filter isNotPcSet insts
      in BB insts_wo_PCUpd tr
    
    _ -> bb
  
instOrder ii = work [] ii
 where 
  work accum [] = reverse accum
  work accum [i] = work (i:accum) [] 
  work accum (i1:i2:insts) = 
    let (defs1, _) = iDefUse i1 
        (_, uses2) = iDefUse i2 
        reshuffle =
          Set.size (Set.intersection defs1 uses2) == 0  
          && case (instructionType i1, instructionType i2) of 
                (LabelSpecificInstruction, RegularInstruction RegDestructor) -> True 
                (LabelSpecificInstruction, RegularInstruction RegOther) -> True 
                (RegularInstruction RegConstructor, LabelSpecificInstruction) -> True
                _ -> False
    in if reshuffle then 
         case accum of 
            p : prevs -> 
              work prevs (p:i2:i1:insts)
            [] -> 
              work [i2] (i1:insts)
       else 
         work (i1:accum) (i2:insts)



instance PEval RawBBTree where
  peval bb@(BB insts tr) = do 
    (BB jj tr'', used) <- listen $ do 
        ii <- Data.Maybe.catMaybes <$> mapM pevalInst insts
        tr' <- peval tr 
        return $ BB ii tr'
    let (insts_no_ret, set_pc_bl) = filterInstBwd (filter (isLiveInstFwd used) jj)
    let BB insts_ bb_ =
          case tr'' of 
            If x (BB i_then tr_then) (BB i_else tr_else) -> 
              BB insts_no_ret $ 
                If x (BB (set_pc_bl ++ i_then) tr_then) 
                     (BB (set_pc_bl ++ i_else) tr_else)
 
            _ -> hoistCalls $ BB (insts_no_ret ++ set_pc_bl) tr''
    let insts_sorted = instOrder insts_ 
    return $ BB insts_sorted bb_
  


funopt :: FunDef -> FunDef 
funopt (FunDef hfn consts bb ir) =  
  
  let constTypes = foldl (\m (x, lit)  -> 
                              case typeOfLit lit of 
                                 Just t -> Map.insert x t m
                                 Nothing -> m 
                          ) Map.empty consts 

      pstate = PState {stateMon = Map.empty, 
                       stateLVals = Map.empty,
                       stateJoins = Map.empty,
                       stateSubst = Subst (Map.empty),
                       stateChange = False,
                       stateTypes = constTypes
                       }
      (bb', _, _) = runRWS (peval bb) () pstate
      new = FunDef hfn consts bb' ir  
  in if bb /= bb' then funopt new else new



class RawOptable a where 
  rawopt :: a -> a 


instance RawOptable RawProgram where   
  rawopt (RawProgram atoms fdefs) = 
      RawProgram (rawopt atoms)  (map rawopt fdefs)

instance RawOptable FunDef where 
  rawopt = funopt 

instance RawOptable Core.Atoms where 
  rawopt = id 

instance RawOptable RawUnit where 
  rawopt (FunRawUnit f) = FunRawUnit (rawopt f)
  rawopt (AtomRawUnit c) = AtomRawUnit (rawopt c)
  rawopt (ProgramRawUnit p) = ProgramRawUnit (rawopt p)
