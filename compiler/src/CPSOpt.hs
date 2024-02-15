{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-- Obs: 2018-02-16: beacuse of the RetCPS representation, we currently
have very few rewrites that actually kick-in; we should be able to
rectify them with some more work, but that's postponed for now; AA
--}


module CPSOpt (rewrite) where

-- todo: consider renaming this to CPSRewrite

import Debug.Trace
import qualified Basics
import RetCPS as CPS
import qualified Core as C
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Data.List

import Data.Map.Lazy(Map)

import qualified Data.Map.Lazy as Map

import Control.Monad.Trans.Maybe
import Control.Monad.Identity ()

import Data.Set (Set)

import qualified Data.List 
import qualified Data.Maybe 

import qualified Data.Set as Set
import RetFreeVars as FreeVars
import TroupePositionInfo



newtype Subst = Subst (Map VarName VarName)

class Substitutable a where
  apply :: Subst -> a -> a

idSubst :: Subst
idSubst = Subst (Map.empty)

instance Substitutable KLambda where
  apply subst@(Subst varmap) kl =
    case kl of
      Unary vn kt ->
        let subst' = Subst (Map.delete vn varmap)
        in  Unary vn (apply subst' kt)
      Nullary kt ->
        let subst' = Subst (varmap)
        in Nullary (apply subst' kt)


instance Substitutable SVal where
  apply _ (Lit lit) = Lit lit
  apply subst (KAbs klambda) = KAbs (apply subst klambda)


instance Substitutable SimpleTerm where
  apply subst@(Subst varmap) simpleTerm =
    case simpleTerm of
      Bin op v1 v2 -> Bin op (fwd v1) (fwd v2)
      Un op v -> Un op (fwd v)
      Tuple vs -> Tuple (map fwd vs)
      Record fields -> Record $ fwdFields fields
      WithRecord x fields -> WithRecord (fwd x) $ fwdFields fields
      ProjField x f -> ProjField (fwd x) f
      ProjIdx x idx -> ProjIdx (fwd x) idx
      List vs -> List (map fwd vs)
      ListCons v v' -> ListCons (fwd v) (fwd v')
      ValSimpleTerm sv -> ValSimpleTerm (apply subst sv)
      Base v -> Base v
      Lib l v -> Lib l v 
    where fwd x = Map.findWithDefault x x varmap
          fwdFields fields = map (\(f, x) -> (f, fwd x)) fields

instance Substitutable ContDef where
  apply subst@(Subst varmap) (Cont vn kt) =
     let subst' = Subst (Map.delete vn varmap)
     in Cont vn (apply subst' kt)

instance Substitutable FunDef where
  apply subst@(Subst varmap) (Fun vn klam) =
    let subst' = Subst (Map.delete vn varmap)
    in Fun vn (apply subst' klam)

instance Substitutable KTerm where
  apply subst@(Subst varmap) kontTerm =
    case kontTerm of
      LetSimple x st kt ->
        LetSimple (vfwd x) (apply subst st) (apply subst kt)
      LetRet kdef@(Cont _ _) kt ->
        let kdef' = apply subst kdef
            kt'   = apply subst kt
        in LetRet kdef' kt'
      LetFun fdefs kt ->
         let fnames = map (\(Fun v _) -> v) fdefs
             subst' = Subst ( foldl (\m v -> Map.delete v m) varmap fnames)
             kt' = apply subst' kt
             fdefs' = map (apply subst') fdefs
         in LetFun fdefs' kt'
      Halt v -> Halt (vfwd v)
      KontReturn v  -> KontReturn (vfwd v)
      ApplyFun fn argn -> ApplyFun (vfwd fn) (vfwd argn)
      If v k1 k2 -> If (vfwd v) (apply subst k1) (apply subst k2)
      AssertElseError v k1 z p -> AssertElseError (vfwd v) (apply subst k1) (vfwd z) p
      Error x p -> Error (vfwd x) p
   where vfwd x = Map.findWithDefault x x varmap


type Census = Map VarName Integer

type CensusCollector = State Census 

class CensusCollectible a 
  where updateCensus :: a -> CensusCollector ()

incUse :: VarName -> CensusCollector ()
incUse x = modify $ Map.insertWith (+) x 1 

instance CensusCollectible VarName where
  updateCensus = incUse 

instance CensusCollectible a => CensusCollectible [a] where 
  updateCensus = mapM_ updateCensus

instance CensusCollectible SimpleTerm where 
  updateCensus t = case t of 
      Bin _ v1 v2 -> updateCensus [v1,v2]
      Un _ v -> updateCensus v 
      ValSimpleTerm sv -> updateCensus sv 
      Tuple vs -> updateCensus vs 
      Record fs -> let (_,vs) = unzip fs in updateCensus vs 
      WithRecord v fs -> updateCensus v >> (let (_,vs) = unzip fs in updateCensus vs )
      ProjField v _ -> updateCensus v
      ProjIdx v _ -> updateCensus v
      List vs -> updateCensus vs
      ListCons v vs -> updateCensus v >> updateCensus vs
      Base _ -> return () 
      Lib _ _ -> return ()

instance CensusCollectible KLambda where   
  updateCensus kl = case kl of 
      Unary _ kt -> updateCensus kt 
      Nullary kt -> updateCensus kt 

instance CensusCollectible SVal where 
  updateCensus sv = case sv of 
    KAbs kl -> updateCensus kl 
    Lit _ -> return ()

instance CensusCollectible ContDef where 
  updateCensus (Cont _ kt) = updateCensus kt 

instance CensusCollectible FunDef where 
  updateCensus (Fun _ kl) = updateCensus kl 

instance CensusCollectible KTerm where 
  updateCensus t = case t of 
    LetSimple _ st kt -> updateCensus st >> updateCensus kt 
    LetFun fs kt -> updateCensus fs >> updateCensus kt 
    LetRet ct kt -> updateCensus ct >> updateCensus kt 
    KontReturn x -> updateCensus x
    ApplyFun v u -> updateCensus [v,u]
    If v k1 k2 -> updateCensus v >> updateCensus [k1,k2]
    AssertElseError v k u _ -> updateCensus [v,u] >> updateCensus k 
    Error v _ -> updateCensus v 
    Halt v -> updateCensus v

  
getCensus :: KTerm -> Census 
getCensus k = execState (updateCensus k) Map.empty



---------------------------

data Term = St SimpleTerm | Unknown deriving (Eq,Show) 
type Env = Map VarName Term


class BindableDef a where 
  binddef::a -> Opt () 


bindenv :: VarName -> Term -> Opt ()
bindenv x t = 
 modify (\s -> s { __env_of_state = Map.insert x t (__env_of_state s) })

-- instance BindableDef FunDef where 
--   binddef (Fun v kl) = bindenv v (Fn kl)

instance BindableDef a => BindableDef [a] where 
  binddef = mapM_ binddef 

--------------------

type CSEMap = Map SimpleTerm VarName

data OptState = OptState {
      __env_of_state :: Env
    }

data OptReader = OptReader {
      __census_of_reader :: Census ,
      __rewrite_ret_of_reader :: Maybe ContDef ,
      __cse_map_of_reader :: CSEMap
    }    

type Opt = RWS OptReader () OptState
class Simplifiable a where 
  simpl :: a -> Opt a

instance Simplifiable a => Simplifiable [a] where 
  simpl = mapM simpl

instance Simplifiable FunDef where 
  simpl (Fun arg kl) = simpl kl  >>= return . Fun arg

instance Simplifiable ContDef where 
  simpl (Cont v kt) = simpl kt >>= return . Cont v
    
instance Simplifiable KLambda where 
  simpl (Unary v k) = simpl k >>= return . Unary v 
  simpl (Nullary k) = simpl k >>= return . Nullary

look :: VarName -> Opt Term 
look x = do 
  m <- __env_of_state <$> get 
  return $ Map.findWithDefault Unknown
              -- (error $ "cannot find binding for name" ++ (show x)) 
              x m

censusInfo :: VarName -> Opt Integer
censusInfo x = do 
  census <- __census_of_reader <$> ask 
  return $ Map.findWithDefault 0 x census


fields x = do 
    w <- look x 
    case w of 
      St (Record xs) -> return xs 
      St (WithRecord y ys) ->  do
        xs <- fields y 
        return $ xs ++ ys 
      _ -> return []


isRecordTerm (St (Record _)) = True 
isRecordTerm (St (WithRecord _ _ )) = True 
isRecordTerm _ = False

recordEquiv r1 r2 = do 
  f1 <- fields r1 
  f2 <- fields r2 
  let f1' = sort f1 
      f2' = sort f2 
  return (f1' == f2')



data ResOrSubst a = ResultSimplified a | ResultSubst VarName 
simplifySimpleTerm :: SimpleTerm -> Opt (ResOrSubst SimpleTerm)
simplifySimpleTerm t = 
  let _ret = return. ResultSimplified 
      _subst = return . ResultSubst 
      _nochange = _ret t
  in case t of     
  Bin op oper1 oper2 -> do 
    u <- look oper1 
    v <- look oper2
    case op of 
      Basics.HasField -> case v of 
           St (ValSimpleTerm  (Lit (C.LString s))) -> do 
             fs <- fields oper1 
             case lookup s fs of 
               Just _ -> _ret $ __trueLit 
               Nothing -> _nochange
           _ -> _nochange

      -- Basics.Eq | (isLit u && isLit v) -> 
                    -- _ret $ lit $ C.LBool (litVal u == litVal v) -- slightly more general case
      Basics.Eq | u == v  && (u /= Unknown) -> _ret $ __trueLit 
      Basics.Eq | (isLit u && isLit v) -> _ret $ lit $ C.LBool (litVal u == litVal v) 
      Basics.Eq | isRecordTerm u -> do 
            e <- recordEquiv oper1 oper2 
            if e then _ret $ __trueLit 
                 else _nochange
      Basics.Neq | isLit u && isLit v -> _ret $ lit $ C.LBool (litVal u /= litVal v)
      
      _ -> case (u, v) of 
              (St (ValSimpleTerm (Lit (C.LInt n1 _))), 
               St (ValSimpleTerm (Lit (C.LInt n2 _)))) ->
                    let ii f = _ret $ lit (C.LInt (f n1 n2) NoPos )
                        bb f = _ret $ lit (C.LBool (f n1 n2)) 
                      in case op of 
                            Basics.Plus  -> ii (+)
                            Basics.Minus -> ii (-)
                            Basics.Mult  -> ii (*)
                            Basics.Le -> bb (<=)
                            Basics.Lt -> bb (<)
                            Basics.Ge -> bb (>=)
                            Basics.Gt -> bb (>)
                            _ -> _nochange 

        
              _ -> _nochange 
  Un op operand -> do
    v <- look operand 
    -- TODO should write out all cases
    case (op,v) of 
        (Basics.IsTuple, St (Tuple _))          -> _ret __trueLit 
        (Basics.IsTuple, St (Record _))         -> _ret __falseLit
        (Basics.IsTuple, St (WithRecord _ _))   -> _ret __falseLit
        (Basics.IsTuple, St (List _))           -> _ret __falseLit
        (Basics.IsTuple, St (ListCons _ _))     -> _ret __falseLit
        (Basics.IsTuple, St (ValSimpleTerm _))  -> _ret __falseLit


        (Basics.IsRecord, St (Record _))        -> _ret __trueLit
        (Basics.IsRecord, St (WithRecord _ _))  -> _ret __trueLit 
        (Basics.IsRecord, St (Tuple _))         -> _ret __falseLit
        (Basics.IsRecord, St (List _))          -> _ret __falseLit
        (Basics.IsRecord, St (ListCons _ _))    -> _ret __falseLit
        (Basics.IsRecord, St (ValSimpleTerm _)) -> _ret __falseLit


        (Basics.IsList, St (List _))          -> _ret __trueLit
        (Basics.IsList, St (ListCons _ _))    -> _ret __trueLit
        (Basics.IsList, St (Record _))        -> _ret __falseLit
        (Basics.IsList, St (WithRecord _ _))  -> _ret __falseLit 
        (Basics.IsList, St (Tuple _))         -> _ret __falseLit
        (Basics.IsList, St (ValSimpleTerm _)) -> _ret __falseLit

        (Basics.TupleLength, St (Tuple xs)) -> 
            _ret $ lit (C.LInt (fromIntegral (length xs)) NoPos)
        -- 2023-08 Revision: Added this case
        (Basics.ListLength, St (List xs)) -> 
            _ret $ lit (C.LInt (fromIntegral (length xs)) NoPos)
            
   
       
        _ -> _nochange
  ProjField x s ->  do 
    fs <- fields x
    case lookup s fs of 
      Just y -> _subst y 
      Nothing -> _nochange
  ProjIdx x idx -> do
    t <- look x
    case t of
      St (Tuple vs) | fromIntegral (length vs) > idx ->
        _subst (vs !! fromIntegral idx)
      _ -> _nochange


  ValSimpleTerm (KAbs klam) -> do
        klam' <- withResetRetState $ simpl klam
        _ret $ ValSimpleTerm $ KAbs klam'
{--
  List _ -> _nochange 
  ListCons _ _ -> _nochange
  Base _ -> _nochange
  Lib _ _ -> _nochange 
        --}
  _ -> _nochange

  where 
    lit l = ValSimpleTerm  (Lit l)
    isLit (St (ValSimpleTerm (Lit _))) = True 
    isLit _ = False 
    litVal (St (ValSimpleTerm (Lit (C.LInt i _)))) = (C.LInt i NoPos)
    litVal (St (ValSimpleTerm (Lit x))) = x
    litVal _ = error "incorrect application of litVal"
    __trueLit = lit (C.LBool True)
    __falseLit = lit (C.LBool False)


subst x v t = apply (Subst (Map.singleton  x v )) t

withResetRetState = local (\r -> r {__rewrite_ret_of_reader = Nothing}) 
withRetState st = local (\r -> r {__rewrite_ret_of_reader = Just st})

state_info :: Opt String 
state_info = do 
  r <- __rewrite_ret_of_reader <$> ask 
  return $ "ret\n"  ++ (show r)


failFree :: SimpleTerm -> Bool -- 2021-05-19; AA; hack  
failFree st = case st of 
  Bin op _ _ ->  op `elem` [Basics.Eq, Basics.Neq] 
  Un _ _ -> False 
  ValSimpleTerm _ -> True 
  Tuple _ -> True 
  Record _ -> True 
  WithRecord _ _ -> True 
  ProjField _ _ -> False 
  ProjIdx _ _ -> False 
  List _ -> True 
  ListCons _ _ -> False 
  Base _ -> True 
  Lib _ _ -> True 

instance Simplifiable KTerm where 
  simpl k = do 
    --s <- state_info     
    -- trace ("simpl-kterm\n" ++ (s) ++ "\n" ++ "~~~\n" ++(show k)++ ("\n----"))  $ 
    case k of 
      LetSimple x st kt -> do 
        _cse <- __cse_map_of_reader <$> ask 
        case Map.lookup st _cse of 
          Just w -> simpl $ subst x w kt 
          Nothing -> do 
            x_uses <- censusInfo x 
            case (x_uses, st) of 
              (0, _) | failFree st  -> simpl kt 
              (1, ValSimpleTerm (KAbs klambda@(Unary _ _ ))) 
                | isApplied x kt ->  do
                      bindenv x (St st)
                      simpl kt          -- remove the let-declaration
                                        -- expecting the substitution down the 
                                        -- road in the application case 
                                        -- 2021-05-17; AA
              _  -> do      
                w <- simplifySimpleTerm st 
                case w of 
                  ResultSimplified st' -> do
                      bindenv x (St st')
                      kt' <- local (\r -> r { __cse_map_of_reader = Map.insert st' x _cse } ) (simpl kt)
                      return $ LetSimple x st' kt' 
                  ResultSubst w ->                 
                      simpl $ subst x w kt 
      LetFun fdefs kt -> do 
        -- binddef fdefs 
        fdefs' <- withResetRetState $ simpl fdefs 
        kt' <- simpl kt 
        return $ LetFun fdefs' kt' 
      LetRet ret kt -> do 
        ret_now <- __rewrite_ret_of_reader <$> ask 
        ret' <- simpl ret 
        if hasUniqueReturn kt 
          then withRetState ret' (simpl kt)
          else do           
            kt' <- withResetRetState (simpl kt)
            return $ LetRet ret' kt' 
      KontReturn x -> do
        ret <- __rewrite_ret_of_reader <$> ask 
        case ret of 
          Nothing -> return $ KontReturn x
          Just (Cont y kt) -> return $ subst y x kt 
      ApplyFun x y -> do 
        x_uses <- censusInfo x
        case x_uses of 
          1 -> do v <- look x
                  case v of 
                    (St (ValSimpleTerm (KAbs (Unary arg body)))) -> do 
                      simpl $ subst arg y body
                    _ -> return k
          _ -> return k
      If x k1 k2 -> do 
        v <- look x
        case v of 
          St (ValSimpleTerm (Lit (C.LBool b))) -> 
            simpl (if b then k1 else k2)
          _ -> do 
            k1' <- withResetRetState $ simpl k1 
            k2' <- withResetRetState $ simpl k2 
            return $ If x k1' k2'
      AssertElseError x kt y pos -> do
        v <- look x 
        case v of 
          St (ValSimpleTerm (Lit (C.LBool b)))->
            simpl (if b then kt else (Error y pos))
          _ -> do
              k' <- simpl kt 
              return $ AssertElseError x k' y pos 
      Error _  _ -> return k 
      Halt _ -> return k 



hasUniqueReturn :: KTerm -> Bool 
hasUniqueReturn k = 
  case k of 
    KontReturn _              -> True 
    LetSimple _ _ k'          -> hasUniqueReturn k' 
    LetFun _ k'               -> hasUniqueReturn k'
    ApplyFun _ _              -> False 
    If _ _ _                  -> False
    AssertElseError _ k _ _   -> hasUniqueReturn k 
    Halt _                    -> True 
    Error _ _                 -> True     
    LetRet (Cont _ k') _      -> hasUniqueReturn k'

isApplied :: VarName -> KTerm -> Bool 
isApplied f k = 
  case k of 
    KontReturn _ -> False 
    LetSimple _  _ k' -> isApplied f k' 
    LetFun fdefs k' -> 
       or $ (isApplied f k') :
            [ isApplied f k | Fun _ kl <- fdefs, let k = kTermOfLambda kl]
    ApplyFun g _ -> g == f 
    If _ k1 k2 -> isApplied f k1 || isApplied f k2
    AssertElseError  _ k _ _ -> isApplied f k 
    Halt _ -> False 
    Error _ _ -> False 
    LetRet (Cont _ k') k'' -> isApplied f k' || isApplied f k'' 
   where kTermOfLambda (Unary _ k) = k
         kTermOfLambda (Nullary k) = k
    

iter :: KTerm -> KTerm
iter kt = 
      let census = getCensus kt
          (kt', _, _) = runRWS (simpl kt) 
                          OptReader {
                             __census_of_reader = census,
                             __rewrite_ret_of_reader = Nothing,
                             __cse_map_of_reader = Map.empty 

                          }
                          OptState { __env_of_state = Map.empty
                          }
      in if kt == kt' then kt
                      else -- trace ((show kt) ++ ("\n------")) 
                           iter kt' 

rewrite :: Prog -> Prog
rewrite (Prog atoms kterm) = 
 Prog atoms (iter kterm)