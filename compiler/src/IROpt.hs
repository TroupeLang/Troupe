{-# LANGUAGE FlexibleContexts #-}

module IROpt(iropt) where
import IR
import Control.Monad.RWS.Lazy
import Data.Map.Lazy (Map)
import Data.Set(Set)
import qualified Data.Set as Set 
import qualified Basics
import qualified Core                      as C
import Core (Numeric(..))
import           TroupePositionInfo (Located(..), unLoc)

import qualified Data.Map.Lazy as Map 
import           RetCPS                    (VarName (..))


--------------------------------------------------
--  substitutions for IR
--------------------------------------------------
newtype Subst = Subst (Map VarName VarAccess)

class Substitutable a where
  apply :: Subst -> a -> a

instance Substitutable VarAccess where 
    apply _ x@(VarEnv _) = x 
    apply _ x@(VarFunSelfRef) = x 
    apply subst@(Subst varmap) (VarLocal x) = 
        Map.findWithDefault (VarLocal x) x varmap

instance Substitutable IRExpr where 
    apply subst e = 
        case e of 
            Bin op x y -> Bin op (apply subst x) (apply subst y)
            Un op x -> Un op (apply subst x)
            Tuple xs -> Tuple (map (apply subst) xs)
            Record fields -> Record (_ff fields)
            WithRecord x fields -> WithRecord (apply subst x) (_ff fields)
            ProjField x f -> ProjField (apply subst x) f
            ProjIdx x idx -> ProjIdx (apply subst x) idx
            List xs  -> List (map (apply subst) xs)
            ListCons x y -> ListCons (apply subst x) (apply subst y)
            Const x -> Const x
            Base name -> Base name 
            Lib name name' -> Lib name name'
        where _ff fields = map (\(f,x) -> (f, apply subst x)) fields

instance Substitutable IRInst where
    apply subst i =
        case i of
            Assign x e -> Assign x (apply subst e)
            MkFunClosures env funs ->
                let env' = map (\(decVar, y) -> (decVar, apply subst y)) env  -- obs: need only subst in y
                in MkFunClosures env' funs

instance Substitutable IRTerminator where
    apply subst tr =
        case tr of
            TailCall x y -> TailCall (apply subst x) (apply subst y)
            Ret x -> Ret (apply subst x)
            If x bb1 bb2 -> If (apply subst x) (apply subst bb1) (apply subst bb2)
            AssertElseError x bb y ->
                AssertElseError (apply subst x) (apply subst bb) (apply subst y)
            LibExport x -> LibExport (apply subst x)
            Error x -> Error (apply subst x)
            StackExpand decVar bb1 bb2 -> StackExpand decVar (apply subst bb1) (apply subst bb2)

-- Instance for Located wrapper - apply substitution to content, preserve position
instance Substitutable a => Substitutable (Located a) where
    apply subst (Loc pos a) = Loc pos (apply subst a)

instance Substitutable IRBBTree where
    apply subst (BB insts tr) =
        BB (map (apply subst) insts) (apply subst tr)

--------------------------------------------------
-- end of substitutions for IR 
--------------------------------------------------



-- | Partial value.
data PValue = Unknown
            | TupleVal [LVarAccess]
            | NumericConst Numeric
            | BoolConst Bool
            | StringConst String
            | RecordVal LFields
             
             


type Env = Map VarName PValue
type ChangeFlag = Bool 
type State = (Env, ChangeFlag)
type Writer = Set VarName 
type Opt = RWS () Writer State 


getEnv = do 
    (e, _) <- get 
    return e

envInsert :: VarName -> PValue -> Opt ()
envInsert x v = do 
    (env, flag) <- get 
    let env' = Map.insert x v env
    put (env', flag)

setChangeFlag :: Opt () 
setChangeFlag = do 
    (e, _) <- get 
    put (e, True)

class PEval a where
    peval :: a -> Opt a 


markUsed x = tell $ Set.singleton x -- collect the use of the local
markUsed' (VarEnv _) = return ()
markUsed' (VarFunSelfRef) = return ()
markUsed' (VarLocal x) = markUsed x

-- | Mark a Located VarAccess as used (extracts VarAccess from Located wrapper)
markUsedL' :: LVarAccess -> Opt ()
markUsedL' (Loc _ va) = markUsed' va

-- | Check if an expression can fail at runtime or has side effects
-- This is used to prevent unsound dead code elimination
canFailOrHasEffects :: IRExpr -> Bool
canFailOrHasEffects expr = case expr of
    -- Binary operations that can fail due to type errors
    Bin op _ _ -> case op of
        -- Arithmetic operations can fail if operands are not numbers
        Basics.Plus -> True
        Basics.Minus -> True
        Basics.Mult -> True
        Basics.Div -> True  -- Also division by zero
        Basics.IntDiv -> True
        Basics.Mod -> True
        -- Bitwise operations require numbers
        Basics.BinAnd -> True
        Basics.BinOr -> True
        Basics.BinXor -> True
        Basics.BinShiftLeft -> True
        Basics.BinShiftRight -> True
        Basics.BinZeroShiftRight -> True
        -- String concatenation can fail if operands are not strings
        Basics.Concat -> True
        -- Comparisons that require numbers
        Basics.Le -> True
        Basics.Lt -> True
        Basics.Ge -> True
        Basics.Gt -> True
        -- Boolean operations 
        Basics.And -> True 
        Basics.Or -> True         
        -- Record checking
        Basics.HasField -> True
        -- These are generally safe
        Basics.Eq -> False
        Basics.Neq -> False
        -- Level operations might be safe but conservative
        Basics.LatticeJoin -> True
        Basics.RaisedTo -> True
    
    -- Unary operations
    Un op _ -> case op of
        -- List/tuple operations can fail
        Basics.Head -> True
        Basics.Tail -> True
        -- Arithmetic
        Basics.UnMinus -> True
        -- Length operations can fail 
        Basics.ListLength -> True
        Basics.TupleLength -> True
        Basics.RecordSize -> True
        -- Boolean negation can fail
        Basics.Not -> True
        -- Type tests are safe
        Basics.IsTuple -> False
        Basics.IsList -> False
        Basics.IsRecord -> False
        -- Level operations
        Basics.LevelOf -> False
    
    -- Field/index projections can fail
    ProjField _ _ -> True
    ProjIdx _ _ -> True
    
    -- List operations
    ListCons _ _ -> True  -- Second argument must be a list
    
    -- Function calls can have side effects
    Base _ -> True
    Lib _ _ -> True
    
    -- These are generally safe
    Tuple _ -> False
    Record _ -> False
    WithRecord _ _ -> False  -- Assuming the base is a record
    List _ -> False
    Const _ -> False 

-- | Get evaluation of a variable.
varPEval :: VarAccess -> Opt PValue
varPEval (VarEnv _) = return Unknown
varPEval (VarFunSelfRef) = return Unknown
varPEval (VarLocal x) = do
    env <- getEnv
    markUsed x
    case Map.lookup x env of
        Just v -> return v
        Nothing -> return Unknown

-- | Get evaluation of a Located VarAccess (extracts VarAccess from Located wrapper)
varPEvalL :: LVarAccess -> Opt PValue
varPEvalL (Loc _ va) = varPEval va


data IRExprRes 
    = RExpr (PValue, IRExpr)
    | RMov VarAccess


        
irExprPeval :: IRExpr -> Opt IRExprRes -- (PValue, IRExpr)
irExprPeval e =
    let r_ x = return (RExpr x)
        def_ = r_ (Unknown, e) in
    case e of
        Un Basics.IsTuple x -> do
            v <- varPEvalL x
            case v of
                TupleVal _ -> do
                    setChangeFlag
                    r_ (BoolConst True, Const (C.LBool True))
                _ -> def_
        Un Basics.IsRecord x -> do
            v <- varPEvalL x
            case v of
                RecordVal _ -> do
                    setChangeFlag
                    r_ (BoolConst True, Const (C.LBool True))
                _ -> def_

        Un Basics.Not x -> do
            v <- varPEvalL x
            case v of
                BoolConst True -> do
                    setChangeFlag
                    r_ (BoolConst False, Const (C.LBool False))
                BoolConst False -> do
                    setChangeFlag
                    r_ (BoolConst True, Const (C.LBool True))
                _ -> def_

        Bin Basics.Eq x y -> do
            v1 <- varPEvalL x
            v2 <- varPEvalL y
            case (v1, v2) of
                (NumericConst a, NumericConst b) | a == b -> do
                    setChangeFlag
                    r_ (BoolConst True, Const (C.LBool True))
                (NumericConst a, NumericConst b) | a /= b -> do
                    setChangeFlag
                    r_ (BoolConst False, Const (C.LBool False))
                _ -> r_ (Unknown, e)


        Bin Basics.HasField x y -> do
            v1 <- varPEvalL x
            v2 <- varPEvalL y
            case (v1, v2) of
                (RecordVal fs, StringConst s) ->
                    case lookup s (map (\(f, Loc _ va) -> (f, va)) fs) of
                        Just _ -> do
                            setChangeFlag
                            r_ (BoolConst True, Const (C.LBool True))
                        Nothing -> def_
                _ -> def_


        Bin op x y -> do
          u <- varPEvalL x
          v <- varPEvalL y
          case (u, v) of
            (NumericConst (NumInt a), NumericConst (NumInt b)) -> do
                let ii f = let c = f a b in do
                              setChangeFlag
                              r_ (NumericConst (NumInt c), Const (C.LNumeric (NumInt c)))
                let bb f = let c = f a b in do
                              setChangeFlag
                              r_ (BoolConst c, Const (C.LBool c))
                case op of
                            Basics.Plus ->  ii (+)
                            Basics.Minus -> ii (-)
                            Basics.Mult ->  ii (*)
                            Basics.Div ->   def_ -- do not mess with divisions -- ii div
                            Basics.IntDiv-> def_
                            Basics.Mod ->   def_ -- ii mod
                            -- Basics.Eq ->    bb (==)
                            Basics.Neq ->   bb(/=)
                            Basics.Le ->    bb (<=)
                            Basics.Lt ->    bb (<)
                            Basics.Ge ->    bb ( >= )
                            Basics.Gt ->    bb ( > )
                            _ -> def_

            _ -> do
              markUsedL' x
              markUsedL' y
              def_
        Record fields -> do mapM pevalField fields
                            r_ (RecordVal fields, e)
                            -- def_
            where pevalField (_, x) = markUsedL' x
        WithRecord r fields -> do
                    markUsedL' r
                    mapM (\(_,x) -> markUsedL' x) fields
                    z <- varPEvalL r
                    let fields' = fields ++ ( case z of
                                               RecordVal f0 -> f0
                                               _ -> [] )
                    r_ (RecordVal fields', e)
        ProjField x s -> do
            v <- varPEvalL x
            case v of
                RecordVal fs ->
                    case lookup s fs of
                        Just (Loc _ y) -> do
                            setChangeFlag
                            return $ RMov y
                            -- r_ (BoolConst True, Const (C.LBool True))
                        Nothing -> def_
                _ -> def_
        -- TODO Implement optimization for ProjIdx
        ProjIdx x idx -> do
            markUsedL' x  -- Mark the tuple variable as used
            def_
        -- ProjIdx x idx -> do 
        --     v <- varPEval x 
        --     case v of 
        --         TupleVal vs -> 
        --         _ -> def_ 

        -- Previous Index:
        -- Bin Basics.Index x y -> do 
        --     v1 <- varPEval x 
        --     v2 <- varPEval y 
        --     case (v1, v2) of 
        --         (TupleVal xs, IntConst i) -> do
        --             setChangeFlag
        --             return $ RMov (xs !! (fromIntegral  i))
        --         _ -> def_

        

-- irExprPeval e@(Bin Basics.Index x y) = do 
--     v1 <- varPEval x 
--     v2 <- varPEval y 
--     case (v1, v2) of 
--         (TupleVal xs, IntConst i) -> 
 
 


        (List xs) -> do
            mapM_ markUsedL' xs
            r_ (Unknown, e)

        (ListCons x y) -> do
            markUsedL' x
            markUsedL' y
            r_ (Unknown, e)    

        (Const x) -> do
            case x of
                C.LNumeric n ->
                    r_ (NumericConst n, e)
                C.LBool b ->
                    r_ (BoolConst b, e)
                C.LString s ->
                    r_ (StringConst s, e)
                _ ->
                    r_ (Unknown, e) 

        (Base _) -> do 
            r_ (Unknown, e)

        (Lib _ _) -> do 
            r_ (Unknown, e)

        (Un Basics.TupleLength x) -> do
            v <- varPEvalL x
            case v of
                TupleVal vars -> do
                    setChangeFlag
                    let n = fromIntegral $ length vars
                    r_ (NumericConst (NumInt n), Const (C.LNumeric (NumInt n)))
                _ -> r_ (Unknown, e)
        -- Not possible as not tracking list content:
        -- (Un Basics.ListLength x) -> do
        --     v <- varPEvalL x
        --     case v of
        --         ListVal -> do

        (Un _ x) -> do
            markUsedL' x
            r_ (Unknown, e)


        (Tuple xs) -> do
            mapM_ markUsedL' xs
            r_ (TupleVal xs, e)


data IRInstRes
    = RIns LIRInst
    | RSubst Subst

-- | Partial evaluation of a Located IR instruction
insPeval :: LIRInst -> Opt IRInstRes
insPeval linst@(Loc pos i) =
    case i of
        Assign x e -> do
            exprRes <- irExprPeval e
            case exprRes of
                RExpr (v', e') -> do
                    envInsert x v'
                    return $ RIns (Loc pos (Assign x e'))
                RMov y ->
                    return $ RSubst $ Subst (Map.singleton x y)
        MkFunClosures envs hfns -> do
            mapM_ (\(_, lx) -> markUsedL' lx) envs
            return $ RIns linst


{--
instance PEval IRInst where
    peval (Assign x e) = do
        RExpr (v', e') <- irExprPeval e
        envInsert x v'
        return (Assign x e')

    peval i@(MkFunClosures envs hfns) = do
        mapM_ (\(_, lx) -> markUsedL' lx) envs
        return i
--}

-- | Partial evaluation of a Located IR terminator
-- Note: IRTerminator now uses LVarAccess, so we use varPEvalL and markUsedL'
trPeval :: LIRTerminator -> Opt IRBBTree

trPeval (Loc pos (If lx bb1 bb2)) = do
        v <- varPEvalL lx
        let _doThen = do setChangeFlag
                         peval bb1

        let _doElse = do setChangeFlag
                         peval bb2
        case v of
            BoolConst True -> _doThen
            BoolConst False -> _doElse
            NumericConst (NumInt x) | x /= 0 -> _doThen
            NumericConst (NumInt 0) -> _doElse

            _ -> do bb1' <- peval bb1
                    bb2' <- peval bb2
                    return $ BB [] (Loc pos (If lx bb1' bb2'))


trPeval (Loc pos (AssertElseError lx bb ly_err)) = do
    v <- varPEvalL lx
    markUsedL' ly_err
    case v of
        BoolConst True -> do
            setChangeFlag
            peval bb
        _ -> do bb' <- peval bb
                return $ BB [] (Loc pos (AssertElseError lx bb' ly_err))


trPeval (Loc pos (StackExpand x bb1 bb2)) = do
    bb1' <- peval bb1
    bb2' <- peval bb2

    case bb1' of
        BB insts1 (Loc _retPos (Ret lrv1)) -> do
            -- Extract VarAccess from LVarAccess for the substitution map key
            let rv1 = unLoc lrv1
            let subst = Subst (Map.singleton x rv1)
            let (BB insts2 tr2) = apply subst bb2'
            setChangeFlag
            return $ BB (insts1 ++ insts2) tr2
        _ ->
            return $ BB [] (Loc pos (StackExpand x bb1' bb2'))

trPeval ltr@(Loc _pos (Ret lx)) = do
    markUsedL' lx
    return $ BB [] ltr

trPeval ltr@(Loc _pos (LibExport lx)) = do
    markUsedL' lx
    return $ BB [] ltr

trPeval ltr@(Loc _pos (Error lx)) = do
    markUsedL' lx
    return $ BB [] ltr

trPeval ltr@(Loc _pos (TailCall lx ly)) = do
    markUsedL' lx
    markUsedL' ly
    return $ BB [] ltr


bbPeval (BB insts tr) = do 
    case insts of 
        [] -> trPeval tr 
        (i:insts) -> do 
            insRes <- insPeval i 
            case insRes of 
                RIns i' -> do
                    BB insts'' tr'' <- bbPeval (BB insts tr)
                    return $ BB (i':insts'') tr''
                RSubst subst -> do 
                    bb_ <- bbPeval (BB insts tr)
                    setChangeFlag
                    return (apply subst bb_)
                    


instance PEval IRBBTree where
    peval bb@(BB insts tr) = do

        (BB insts_ tr_, used) <- listen $ bbPeval bb

        let isNotDeadAssign (Loc _ (Assign x e)) =
                Set.member x used || canFailOrHasEffects e
            isNotDeadAssign _   = True

            instsFiltered = filter isNotDeadAssign insts_
        return $ BB instsFiltered tr_ 



-- | Optimize a Located FunDef
funopt :: LFunDef -> LFunDef
funopt (Loc funDefPos (FunDef hfn largname@(Loc _ argname) consts bb)) =
    let initEnv = (Map.singleton argname Unknown, False)
        (bb', (_, _hasChanges), _) = runRWS (peval bb) () initEnv

        new = Loc funDefPos (FunDef hfn largname consts bb')
    in if (bb /= bb')  then funopt new
                       else new



iropt::IRProgram -> IRProgram
iropt (IRProgram atoms fdefs) = IRProgram atoms (map funopt fdefs)
