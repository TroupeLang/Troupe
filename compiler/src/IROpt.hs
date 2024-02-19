{-# LANGUAGE FlexibleContexts #-}

module IROpt(iropt) where
import IR
import Control.Monad.RWS.Lazy
import Data.Map.Lazy (Map)
import Data.Set(Set)
import qualified Data.Set as Set 
import qualified Basics
import qualified Core                      as C
import           TroupePositionInfo

import qualified Data.Map.Lazy as Map 
import           RetCPS                    (VarName (..))


--------------------------------------------------
--  substitutions for IR
--------------------------------------------------
newtype Subst = Subst (Map VarName VarAccess)

class Substitutable a where
  apply :: Subst -> a -> a

idSubst :: Subst
idSubst = Subst (Map.empty)


instance Substitutable VarAccess where 
    apply _ x@(VarEnv _) = x 
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
            AssertElseError x bb y pos -> 
                AssertElseError (apply subst x) (apply subst bb) (apply subst y) pos 
            LibExport x -> LibExport (apply subst x)
            Error x pos -> Error (apply subst x) pos 
            Call decVar bb1 bb2 -> Call decVar (apply subst bb1) (apply subst bb2)

instance Substitutable IRBBTree where 
    apply subst (BB insts tr) = 
        BB (map (apply subst) insts) (apply subst tr)

--------------------------------------------------
-- end of substitutions for IR 
--------------------------------------------------



-- | Partial value.
data PValue = Unknown
            | TupleVal [VarAccess]
            | ListVal
            | IntConst Integer
            | BoolConst Bool
            | StringConst String
            | RecordVal Fields
             
             


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
markUsed' (VarLocal x) = markUsed x 

-- | Get evaluation of a variable.
varPEval :: VarAccess -> Opt PValue 
varPEval (VarEnv _) = return Unknown
varPEval (VarLocal x) = do 
    env <- getEnv 
    markUsed x
    case Map.lookup x env of 
        Just v -> return v 
        Nothing -> return Unknown


data IRExprRes 
    = RExpr (PValue, IRExpr)
    | RMov VarAccess


        
irExprPeval :: IRExpr -> Opt IRExprRes -- (PValue, IRExpr)
irExprPeval e = 
    let r_ x = return (RExpr x) 
        def_ = r_ (Unknown, e) in
    case e of 
        Un Basics.IsTuple x -> do 
            v <- varPEval x 
            case v of 
                TupleVal _ -> do 
                    setChangeFlag
                    r_ (BoolConst True, Const (C.LBool True))
                _ -> def_
        Un Basics.IsRecord x -> do 
            v <- varPEval x 
            case v of 
                RecordVal _ -> do 
                    setChangeFlag 
                    r_ (BoolConst True, Const (C.LBool True))
                _ -> def_
        

        Bin Basics.Eq x y -> do 
            v1 <- varPEval x 
            v2 <- varPEval y 
            case (v1, v2) of 
                (IntConst a, IntConst b) | a == b -> do
                    setChangeFlag
                    r_ (BoolConst True, Const (C.LBool True))
                (IntConst a, IntConst b) | a /= b -> do
                    setChangeFlag
                    r_ (BoolConst False, Const (C.LBool False))
                _ -> r_ (Unknown, e)
        

        Bin Basics.HasField x y -> do 
            v1 <- varPEval x 
            v2 <- varPEval y 
            case (v1, v2) of 
                (RecordVal fs, StringConst s) -> 
                    case lookup s fs of 
                        Just _ -> do
                            setChangeFlag
                            r_ (BoolConst True, Const (C.LBool True))
                        Nothing -> def_ 
                _ -> def_ 
        
        
        Bin op x y -> do 
          u <- varPEval x 
          v <- varPEval y
          case (u, v) of 
            (IntConst a, IntConst b) -> do
                let ii f = let c = f a b in do
                              setChangeFlag
                              r_ (IntConst c, Const (C.LInt c NoPos))  
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
                            -- _  -> fail "Type error discovered at compliation time"
                            
            _ -> do
              markUsed' x 
              markUsed' y
              def_
        Record fields -> do mapM pevalField fields 
                            r_ (RecordVal fields, e)
                            -- def_
            where pevalField (_, x) = markUsed' x
        WithRecord r fields -> do   
                    markUsed' r
                    mapM (\(_,x) -> markUsed' x) fields
                    z <- varPEval r 
                    let fields' = fields ++ ( case z of 
                                               RecordVal f0 -> f0
                                               _ -> [] )
                    r_ (RecordVal fields', e)
        ProjField x s -> do 
            v <- varPEval x 
            case v of 
                RecordVal fs -> 
                    case lookup s fs of 
                        Just y -> do
                            setChangeFlag
                            return $ RMov y 
                            -- r_ (BoolConst True, Const (C.LBool True))
                        Nothing -> def_ 
                _ -> def_ 
        -- TODO Implement optimization for ProjIdx
        ProjIdx x idx -> def_
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
            mapM_ markUsed' xs
            r_ (Unknown, e)

        (ListCons x y) -> do 
            markUsed' x 
            markUsed' y 
            r_ (Unknown, e)    

        (Const x) -> do 
            case x of 
                C.LInt n pos -> 
                    r_ (IntConst n, e)
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
            v <- varPEval x 
            case v of 
                TupleVal vars -> do  
                    setChangeFlag
                    let n = fromIntegral $ length vars
                    r_ (IntConst n, Const (C.LInt n NoPos))    
                _ -> r_ (Unknown, e)    
        -- Not possible as not tracking list content:
        -- (Un Basics.ListLength x) -> do 
        --     v <- varPEval x 
        --     case v of 
        --         ListVal -> do  

        (Un _ x) -> do 
            markUsed' x 
            r_ (Unknown, e)


        (Tuple xs) -> do
            mapM_ markUsed' xs 
            r_ (TupleVal xs, e)


data IRInstRes 
    = RIns IRInst 
    | RSubst Subst 

insPeval :: IRInst -> Opt IRInstRes 
insPeval i = 
    case i of 
        Assign x e -> do 
            exprRes <- irExprPeval e 
            case exprRes of 
                RExpr (v', e') -> do
                    envInsert x v' 
                    return $ RIns (Assign x e')
                RMov y ->
                    return $ RSubst $ Subst (Map.singleton x y)
        (MkFunClosures envs hfns) -> do 
            mapM (\(_, x) -> markUsed' x) envs 
            return $ RIns i


{--
instance PEval IRInst where 
    peval (Assign x e) = do 
        RExpr (v', e') <- irExprPeval e 
        envInsert x v' 
        return (Assign x e')        

    peval i@(MkFunClosures envs hfns) = do 
        mapM (\(_, x) -> markUsed' x) envs 
        return i
--}

trPeval :: IRTerminator -> Opt IRBBTree

trPeval (If x bb1 bb2) = do 
        v <- varPEval x 
        let _doThen = do setChangeFlag   
                         peval bb1 
        
        let _doElse = do setChangeFlag
                         peval bb2  
        case v of 
            BoolConst True -> _doThen
            BoolConst False -> _doElse
            IntConst x | x /= 0 -> _doThen 
            IntConst 0 -> _doElse
                                  
            _ -> do bb1' <- peval bb1 
                    bb2' <- peval bb2 
                    return $ BB [] (If x bb1' bb2')


trPeval (AssertElseError x bb y_err pos) = do 
    v <- varPEval x 
    markUsed' y_err
    case v of 
        BoolConst True -> do 
            setChangeFlag
            peval bb 
        _ -> do bb' <- peval bb 
                return $ BB [] (AssertElseError x bb' y_err pos)     


trPeval (Call x bb1 bb2) = do 
    bb1' <- peval bb1 
    bb2' <- peval bb2

    case bb1' of 
        BB insts1 (Ret rv1) -> do 
            let subst = Subst (Map.singleton x rv1)
            let (BB insts2 tr2) = apply subst bb2' 
            setChangeFlag           
            return $ BB (insts1 ++ insts2) tr2
        _ -> 
            return $ BB [] (Call x bb1' bb2')

trPeval tr@(Ret x) = do 
    markUsed' x 
    return $ BB [] tr 

trPeval tr@(LibExport x) = do 
    markUsed' x 
    return $ BB [] tr 

trPeval tr@(Error x _) = do 
    markUsed' x 
    return $ BB [] tr 

trPeval tr@(TailCall x y)  = do 
    markUsed' x 
    markUsed' y 
    return $ BB [] tr


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

        let isNotDeadAssign (Assign x _) = Set.member x used 
            isNotDeadAssign _   = True

            instsFiltered = filter isNotDeadAssign insts_
        return $ BB instsFiltered tr_ 



funopt :: FunDef -> FunDef
funopt (FunDef hfn argname consts bb) = 
    let initEnv = (Map.singleton argname Unknown, False)
        (bb', (_, hasChanges), _) = runRWS (peval bb) () initEnv

        new = FunDef hfn argname consts bb'
    in if (bb /= bb')  then funopt new 
                       else new 



iropt::IRProgram -> IRProgram
iropt (IRProgram atoms fdefs) = IRProgram atoms (map funopt fdefs)
