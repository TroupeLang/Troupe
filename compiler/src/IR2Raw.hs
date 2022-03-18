module IR2Raw (ir2raw, prog2raw) where
import qualified IR 
import IR (VarAccess(..))
import Raw 

import           RetCPS                    (VarName (..))
import Control.Monad.Trans.RWS
import qualified Basics
import Data.List (find)
import Data.Map.Lazy (Map, (!))
import qualified Data.Map.Lazy as Map 
import qualified Core as C 


-- translation monad 
type TM = RWS (Map VarName C.Lit ) [RawInst] Int 


intercept = (censor (\_ -> []) . listen)

fresh :: TM RawVar 
fresh = fresh_with "_raw_"

fresh_counter = do 
    i <- get
    put (i + 1)
    return i
    

fresh_lval :: TM VarName 
fresh_lval = do 
    i <- fresh_counter 
    return $ VN ("lval" ++ (show i))

fresh_with:: String -> TM RawVar    
fresh_with s = do 
    i <- fresh_counter 
    return $ RawVar $ s  ++ (show i) 

join = Bin Basics.LatticeJoin



fresh3 = do 
    r1 <- fresh
    r2 <- fresh 
    r3 <- fresh 
    return (r1, r2, r3)

getpc = do 
    pc <- fresh_with "_pc_"   
    tell [AssignRaw pc (ProjectState MonPC) ]    
    return pc


expr2raw:: IR.IRExpr -> TM (RawVar, RawVar, RawVar)
expr2raw (IR.Const lit) = do 
    r1 <- fresh 
    pc <- getpc
    tell [ AssignRaw r1 (Const lit)]  
    return (r1, pc, pc)

expr2raw (IR.Base "$$authorityarg") = 
    varAccess2raw (IR.VarLocal (VN "$$authorityarg"))


expr2raw (IR.Base v) = do    
    pc <- getpc 
    t <- fresh_lval     
    s <- fresh 
    tell [ AssignLVal t $ Base v 
         , AssignRaw s (ProjectLVal (IR.VarLocal t) FieldValue)
         ] 
    return (s, pc, pc)


expr2raw e@(IR.Bin op v1 v2) = do
    (r1, r2, r3) <- varAccess2raw v1 
    (s1, s2, s3) <- varAccess2raw v2 
    if op `elem` 
      [ Basics.Plus, Basics.Minus, Basics.Mult, Basics.Div, Basics.Mod, Basics.IntDiv
      , Basics.Gt, Basics.Lt, Basics.Ge, Basics.Le ]
        then do 
            raiseBlock r3 
            raiseBlock s3
            if (op == Basics.Div) then raisePC s2 else return ()
            (t1, t2, t3 ) <- fresh3
            t4 <- fresh 
            pc <- getpc 
            let asserts = 
                    if op `elem` 
                            [Basics.Gt, Basics.Lt, Basics.Ge, Basics.Le ]
                    then [ AssertEqTypes 
                                (Just (List2OrMore RawNumber RawString [])) r1 s1 ]

                    else [ AssertType r1 RawNumber, AssertType s1 RawNumber ] 
            tell $ 
                 asserts ++ 
                 [ AssignRaw t1 $Bin op r1 s1
                 , AssignRaw t2 $join r2 s2
                 , AssignRaw t3 $join r3 s3
                 , AssignRaw t4 $join t2 pc 
                 ]
            return (t1, t4, pc)
    else case op of      
        Basics.Index -> do
            raiseBlock r3 
            raiseBlock s3
            t0 <- fresh_lval
            
            tell [ AssertType r1 RawTuple 
                 , AssertType s1 RawNumber
                 , AssignLVal t0 $ComplexRaw $ Bin op r1 s1
                 ]
            
            (t1, t2, t3) <- varAccess2raw (IR.VarLocal t0)

            pc <- getpc 
            t2'  <- fresh 
            t2'' <- fresh 
            t2''' <- fresh
            t3'  <- fresh
            t3'' <- fresh 
            t3''' <- fresh 

            tell [ AssignRaw t2' $ join t2 pc
                 , AssignRaw t2'' $ join r2 s2 
                 , AssignRaw t2''' $ join t2' t2''
                 , AssignRaw t3' $ join r3 s3
                 , AssignRaw t3'' $ join t3' pc
                 , AssignRaw t3''' $ join t3'' t3 ]

            return (t1, t2''', t3''')
        Basics.Concat -> do 
            raiseBlock r3 
            raiseBlock s3 
            pc <- getpc
            (t1, t2, t3 ) <- fresh3
            t4 <- fresh 
            tell [ AssertType r1 RawString 
                 , AssertType s1 RawString
                 , AssignRaw t1 $Bin op r1 s1
                 , AssignRaw t2 $join r2 s2
                 , AssignRaw t3 $join r3 s3
                 , AssignRaw t4 $join t2 pc 
                 ]
            return (t1, t4, pc)

        Basics.RaisedTo -> do 
            raiseBlock s3
            (t1, t2, t3) <- fresh3 
            pc <- getpc
            t2' <- fresh 
            t2'' <- fresh 
            
            tell [ AssertType s1 RawLevel 
                 , AssignRaw t2 $ Bin Basics.RaisedTo r2 s1
                 , AssignRaw t2' $join t2 s2 
                 , AssignRaw t2'' $ join t2' pc
                 , AssignRaw t3 $ join r3 pc 
                 ]

            return (r1, t2'', t3) 
        Basics.HasField -> do 
            raiseBlock r3 
            raiseBlock s3 
            t <- fresh 
            t2 <- fresh 
            t2' <- fresh 
            pc <- getpc
            tell [ AssertType r1 RawRecord 
                 , AssertType s1 RawString
                 , AssignRaw t $ Bin Basics.HasField r1 s1
                 , AssignRaw t2 $ join r2 pc 
                 , AssignRaw t2' $ join t2 s2]
            return (t, t2', pc)

        _ ->  error $ "binop translation is not implemented: " ++ (show e)
expr2raw (IR.Tuple vs) = do
    pc <- getpc
    t <- fresh 
    tell [ AssignRaw t $ Tuple vs]
    return (t, pc, pc)

expr2raw (IR.List vs) = do 
    pc <- getpc 
    t <- fresh
    tell [ AssignRaw t $ List vs]
    return (t, pc, pc)

expr2raw (IR.Record fs) = do 
    pc <- getpc 
    t <- fresh 
    tell [ AssignRaw t $ Record fs] 
    return (t, pc, pc)

expr2raw (IR.WithRecord v fs) = do 
    (r1, r2, r3) <- varAccess2raw v 
    pc <- getpc 
    raiseBlock r3
    t <- fresh 
    l <- fresh
    tell [ AssertType r1 RawRecord 
         , AssignRaw t $ WithRecord r1 fs
         , AssignRaw l $ join r2 pc 
         ]
    return (t, l, pc)

expr2raw (IR.Proj v field) = do 
    (r1, r2, r3) <- varAccess2raw v 
    raiseBlock r3 
    t0 <- fresh_lval
    tell [ AssertType r1 RawRecord 
         , AssignLVal t0 $ ComplexRaw $ Proj r1 field 
         ]
    (t1, t2, t3) <- varAccess2raw (IR.VarLocal t0)

    pc <- getpc 
    t2' <- fresh 
    t2'' <- fresh 
    t3' <- fresh 
    tell [ AssignRaw t2' $ join t2 pc 
         , AssignRaw t2'' $ join t2' r2
         , AssignRaw t3' $ join t3 pc
         ]
    return (t1, t2'', t3')

expr2raw (IR.ListCons v1 v2) = do 
    (s1, s2, s3) <- varAccess2raw v2
    pc <- getpc 
    raiseBlock s3
    t <- fresh  
    l <- fresh 
    tell [ AssertType s1 RawList 
         , AssignRaw t $ ListCons v1 s1
         , AssignRaw l $ join s2 pc
         ]
    return (t, l, pc)


expr2raw (IR.Un uop va) = do 
    pc <- getpc
    (r1, r2, r3) <- varAccess2raw va     
    if uop `elem` [ Basics.IsTuple
                  , Basics.Length
                  , Basics.IsList
                  , Basics.Tail
                  , Basics.IsRecord
                  ]
      then do 
                t <- fresh
                t' <- fresh 
                raiseBlock r3
                tell $ case uop of 
                             Basics.Tail -> [AssertType r1 RawList] 
                             _ -> []

                tell [ AssignRaw t $ Un uop r1
                     , AssignRaw t' $ join r2 pc 
                     ]
                return (t, t', pc)
      else  
        case uop of 
            Basics.Head -> do
                raiseBlock r3 
                t0 <- fresh_lval 
                tell [ AssertType r1 RawList 
                     , AssignLVal t0 $ ComplexRaw $ Un uop r1]
                (t1, t2, t3) <- varAccess2raw (IR.VarLocal t0)
                pc <- getpc 
                t2' <- fresh
                t2'' <- fresh  
                t3' <- fresh 
                tell [ AssignRaw t2' $ join t2 pc 
                     , AssignRaw t2'' $ join t2 r2  
                     , AssignRaw t3' $ join t3 pc]
                return (t1, t2'', t3')
            Basics.UnMinus -> do 
                raiseBlock r3 
                tell [ AssertType r1 RawNumber ] 
                t1' <- fresh
                t2' <- fresh 
                t3' <- fresh 
                tell [ AssignRaw t1' (Un Basics.UnMinus r1)
                     , AssignRaw t2' $ join r2 pc 
                     , AssignRaw t3' $ join r3 pc ]
                return (t1', t2', t3')
            _ ->
                error $ "unop ?? not implemented: " ++ (show uop)
expr2raw (IR.Lib libname funname) = do 
    t0 <- fresh_lval 
    tell [AssignLVal t0 $ ComplexRaw $  Lib libname funname]
    (t1, t2, t3 ) <- varAccess2raw (IR.VarLocal t0)
    pc <- getpc 
    t2' <- fresh 
    t3' <- fresh 
    tell [ AssignRaw t2' $ join t2 pc 
         , AssignRaw t3' $ join t3 pc ]
    return (t1, t2', t3')

-- expr2raw e = error $ "expr translation is not implemented: " ++ (show e)

pcTaint (r2, r3) = do
    pc <- getpc
    r2' <- fresh 
    r3' <- fresh 
    tell [ AssignRaw r2' $ join pc r2 
         , AssignRaw r3' $ join pc r3 ]    
    return (r2', r3')


inst2raw :: IR.IRInst -> TM ()
inst2raw (IR.Assign vn (IR.Bin op va1 va2)) 
    | op `elem` [Basics.Eq, Basics.Neq]= 
        tell [AssignLVal vn $ ComplexBin op va1 va2]

inst2raw (IR.Assign vn expr) = do
    (r1, r2, r3) <- expr2raw expr
    (r2', r3') <- pcTaint (r2, r3)
    tell [ AssignLVal vn $ ConstructLVal r1 r2' r3' ]

inst2raw (IR.MkFunClosures vs env) = do 
    tell [MkFunClosures vs env]
    


lookupConst :: VarAccess -> TM (Maybe RawVar)
lookupConst x = 
    case x of 
        VarLocal v@(VN w) -> do 
            consts <- ask 
            if Map.member v consts then return $ Just $ RawVar w 
            else return Nothing             
        _ -> return Nothing 

varAccess2raw :: VarAccess -> TM (RawVar, RawVar, RawVar)
varAccess2raw va = do
{--
  _lookupConst <- lookupConst va 
  case _lookupConst of 
      Just rv -> do 
             pc <- getpc 
             return (rv, pc, pc )
      Nothing -> do 
 --}
            raw_val <- fresh_with "_val_"
            raw_vlev <- fresh_with "_vlev_"
            raw_tlev <- fresh_with "_tlev_"
            
            mapM_ (\(r, f) -> tell [AssignRaw r (ProjectLVal va f)]) 
                    [(raw_val, FieldValue), 
                        (raw_vlev, FieldValLev),
                        (raw_tlev, FieldTypLev)]
            
            return (raw_val, raw_vlev, raw_tlev)


raisePC :: RawVar -> TM () 
raisePC x = do 
  pc  <- fresh_with "_pc_"
  bl  <- fresh_with "_bl_"
  pc' <- fresh_with "_pc_"
  bl' <- fresh_with "_bl_"  
  tell [ AssignRaw pc (ProjectState MonPC)    -- get current pc   
       , AssignRaw bl (ProjectState MonBlock) -- get current block
       , AssignRaw pc' (join pc x) 
       , AssignRaw bl' (join bl x) 
       , SetState MonPC pc'
       , SetState MonBlock bl' 
       ]


raiseBlock x = do 
  bl  <- fresh_with "_bl_"
  bl' <- fresh_with "_bl_"  
  tell [ AssignRaw bl (ProjectState MonBlock) -- get current block
       , AssignRaw bl' (Bin Basics.LatticeJoin bl x) 
       , SetState MonBlock bl'
       ]


tr2raw :: IR.IRTerminator -> TM RawTerminator
tr2raw (IR.TailCall v1 v2) = do 
    (r1,r2,r3) <- varAccess2raw v1
    raisePC r2  
    (r1', r2', r3') <- varAccess2raw v2 
    tell [ AssertType r1 RawFunction
         , SetState R0_Val r1'
         , SetState R0_Lev r2'
         , SetState R0_TLev r3']
    return $ TailCall r1 


tr2raw (IR.Ret v) = do 
    (r1, r2, r3) <- varAccess2raw v 
    (r2', r3') <- pcTaint (r2, r3)
    tell [ SetState R0_Val r1
         , SetState R0_Lev r2'
         , SetState R0_TLev r3'
         ]
--    x <- fresh_lval 
--    tell [ AssignLVal x $ ConstructLVal r1 r2' r3' ]
    return Ret


tr2raw (IR.LibExport x) = 
    return $ LibExport x 
    
tr2raw (IR.If v bb1 bb2) = do 
    (r1,r2,r3) <- varAccess2raw v
    raisePC r2
    bb1' <- tree2raw bb1
    bb2' <- tree2raw bb2
    tell [ AssertType r1 RawBoolean ]
    tell [ SetBranchFlag ]
    return $ If r1 bb1' bb2'


tr2raw (IR.Call v bb1 bb2) = do 
    BB insts tr <- tree2raw bb1 
    BB insts' tr' <- tree2raw bb2
    -- saved_pc <- getpc
    let bb1' = BB insts tr
    r0_val <- fresh_with "_r0_val_"
    r0_lev <- fresh_with "_r0_lev_"
    r0_tlev <- fresh_with "_r0_tlev_"
    let ret_prologue = 
            [ AssignRaw r0_val (ProjectState R0_Val)
            , AssignRaw r0_lev (ProjectState R0_Lev)
            , AssignRaw r0_tlev (ProjectState R0_TLev)
            , AssignLVal v $ ConstructLVal r0_val r0_lev r0_tlev 
            -- , SetState MonPC saved_pc
            ] 
    let bb2' = BB ( ret_prologue ++ insts') tr' 
    return $ Call bb1' bb2'  

tr2raw (IR.AssertElseError v1 bb verr pos) = do 
    (r1,r2,r3) <- varAccess2raw v1
    -- (s1,s2,s3) <- varAccess2raw verr
    raiseBlock r2
    -- raiseBlock s3
    bb' <- tree2raw bb
    (tr_err, insts_err) <- intercept $ tr2raw ( IR.Error verr pos )
    let bb_err = BB insts_err tr_err 
    tell [ AssertType r1 RawBoolean ]
    return $ If r1 bb' bb_err


tr2raw (IR.Error verr pos) = do 
       (s1,s2,s3) <- varAccess2raw verr 
       raisePC s2
       raiseBlock s3 
       return $ (Error s1 pos )
       
       


tree2raw :: IR.IRBBTree -> TM RawBBTree 
tree2raw (IR.BB insts tr) = do
    (tr', insts') <- intercept
            $ mapM_ inst2raw insts >> tr2raw tr           
    return $ BB insts' tr'


fun2raw :: IR.FunDef -> FunDef 
fun2raw irfdef@(IR.FunDef hfn vname consts tree) = 
    let (consts_raw, consts_lval) = trConsts consts
        r0_val = RawVar "_$reg0_val"
        r0_lev = RawVar "_$reg0_lev"
        r0_tlev = RawVar "_$reg0_tlev" 
        pc_init = RawVar "_pc_init"
        
        (BB insts tr, _) = evalRWS (tree2raw tree) (Map.fromList  consts) 0 
        ii =  [ AssignRaw r0_val $ ProjectState R0_Val 
              , AssignRaw r0_lev $ ProjectState R0_Lev 
              , AssignRaw r0_tlev $ ProjectState R0_TLev 
              , AssignLVal vname $ ConstructLVal r0_val r0_lev r0_tlev
              , AssignRaw pc_init $ ProjectState MonPC 
              ]
              ++ (map (\f -> f pc_init) consts_lval)
              ++ insts 
        
    in FunDef hfn consts_raw (BB ii tr) irfdef

trConsts consts = 
  let _ls = map (\(lval@(VN x), c) -> 
                   let raw_const = RawVar $ x ++ "$$$const"
                   in
                     ((raw_const, c), 
                      \pc -> AssignLVal lval $ ConstructLVal raw_const pc pc 
                      )) consts
  in unzip _ls
      

ir2raw :: IR.SerializationUnit -> RawUnit 
ir2raw (IR.FunSerialization f) = FunRawUnit (fun2raw f) 
ir2raw (IR.AtomsSerialization c) = AtomRawUnit c 
ir2raw (IR.ProgramSerialization prog) = ProgramRawUnit (prog2raw prog)

prog2raw :: IR.IRProgram -> RawProgram
prog2raw (IR.IRProgram atoms funs) = 
    RawProgram atoms (map fun2raw funs)


