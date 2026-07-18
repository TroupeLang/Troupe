{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-- Obs: 2018-02-16: beacuse of the RetCPS representation, we currently
have very few rewrites that actually kick-in; we should be able to
rectify them with some more work, but that's postponed for now; AA
--}


module CPSOpt (rewrite) where

-- todo: consider renaming this to CPSRewrite

import InternalError (internalError)
import qualified Basics
import RetCPS as CPS
import qualified Core as C
import Core (Numeric(..))
import Control.Monad.RWS
import Control.Monad.State
import Data.List

import Data.Map.Lazy(Map)

import qualified Data.Map.Lazy as Map

import Control.Monad.Identity ()

import TroupePositionInfo (Located(..), unLoc, noLoc)

-- 2025-06-23: AA+cc
-- Helper function to get the last occurrence of a key in an association list.
-- This is needed for correct record field resolution when duplicate field names exist.
-- In Troupe, record field semantics follow "last assignment wins" (e.g., {x=100, x=200}.x should return 200),
-- but Haskell's standard 'lookup' returns the first match. This function ensures we get the last match
-- by reversing the list before lookup, which gives us the rightmost (last) field value.
lookupLast :: Eq a => a -> [(a, b)] -> Maybe b
lookupLast key pairs = lookup key (reverse pairs)



newtype Subst = Subst (Map VarName VarName)

class Substitutable a where
  apply :: Subst -> a -> a

instance Substitutable KLambda where
  apply subst@(Subst varmap) kl =
    case kl of
      Unary lv@(Loc _ vn) lkt ->
        let subst' = Subst (Map.delete vn varmap)
        in  Unary lv (apply subst' lkt)
      Nullary lkt ->
        let subst' = Subst (varmap)
        in Nullary (apply subst' lkt)


instance Substitutable SVal where
  apply _ (Lit lit) = Lit lit
  apply subst (KAbs klambda) = KAbs (apply subst klambda)


instance Substitutable SimpleTerm where
  apply subst@(Subst varmap) simpleTerm =
    case simpleTerm of
      -- Now using LVarName (Located VarName), need to preserve the Located wrapper
      Bin op lv1 lv2 -> Bin op (fwdL lv1) (fwdL lv2)
      Un op lv -> Un op (fwdL lv)
      Tuple lvs tag -> Tuple (map fwdL lvs) tag
      Record fields -> Record (fwdLFields fields)
      WithRecord lx fields -> WithRecord (fwdL lx) (fwdLFields fields)
      ProjField lx f -> ProjField (fwdL lx) f
      ProjIdx lx idx -> ProjIdx (fwdL lx) idx
      List lvs -> List (map fwdL lvs)
      ListCons lv lv' -> ListCons (fwdL lv) (fwdL lv')
      ValSimpleTerm sv -> ValSimpleTerm (apply subst sv)
      Base v -> Base v
      Lib l v -> Lib l v
    where fwd x = Map.findWithDefault x x varmap
          -- Forward a Located VarName, preserving the position
          fwdL (Loc pos vn) = Loc pos (fwd vn)
          -- Forward fields with Located VarNames
          fwdLFields fields = map (\(f, lx) -> (f, fwdL lx)) fields

instance Substitutable LSimpleTerm where
  apply subst (Loc p st) = Loc p (apply subst st)

instance Substitutable ContDef where
  apply subst@(Subst varmap) (Cont vn lkt) =
     let subst' = Subst (Map.delete vn varmap)
     in Cont vn (apply subst' lkt)

instance Substitutable FunDef where
  apply subst@(Subst varmap) (Fun vn klam) =
    let subst' = Subst (Map.delete vn varmap)
    in Fun vn (apply subst' klam)

instance Substitutable (Located FunDef) where
  apply subst (Loc p fd) = Loc p (apply subst fd)

instance Substitutable KTerm where
  apply subst@(Subst varmap) kontTerm =
    case kontTerm of
      LetSimple x lst lkt ->
        LetSimple (vfwd x) (apply subst lst) (apply subst lkt)
      LetRet kdef@(Cont _ _) lkt ->
        let kdef' = apply subst kdef
            lkt'   = apply subst lkt
        in LetRet kdef' lkt'
      LetFun lfdefs lkt ->
         let fnames = map (\(Loc _ (Fun v _)) -> v) lfdefs
             subst' = Subst ( foldl (\m v -> Map.delete v m) varmap fnames)
             lkt' = apply subst' lkt
             lfdefs' = map (apply subst') lfdefs
         in LetFun lfdefs' lkt'
      Halt v -> Halt (vfwd v)
      KontReturn v -> KontReturn (vfwd v)
      ApplyFun fn argn -> ApplyFun (vfwd fn) (vfwd argn)
      If v lk1 lk2 -> If (vfwd v) (apply subst lk1) (apply subst lk2)
      AssertElseError v lk1 z -> AssertElseError (vfwd v) (apply subst lk1) (vfwd z)
      Error x -> Error (vfwd x)
   where vfwd x = Map.findWithDefault x x varmap

instance Substitutable LKTerm where
  apply subst (Loc p kt) = Loc p (apply subst kt)


type Census = Map VarName Integer

type CensusCollector = State Census 

class CensusCollectible a 
  where updateCensus :: a -> CensusCollector ()

incUse :: VarName -> CensusCollector ()
incUse x = modify $ Map.insertWith (+) x 1 

instance CensusCollectible VarName where
  updateCensus = incUse

-- | Instance for Located VarName (extracts VarName from Located wrapper)
instance CensusCollectible LVarName where
  updateCensus (Loc _ vn) = incUse vn

instance CensusCollectible a => CensusCollectible [a] where
  updateCensus = mapM_ updateCensus

instance CensusCollectible SimpleTerm where
  updateCensus t = case t of
      Bin _ v1 v2 -> updateCensus [v1,v2]
      Un _ v -> updateCensus v
      ValSimpleTerm sv -> updateCensus sv
      Tuple vs _ -> updateCensus vs
      Record fs -> let (_,vs) = unzip fs in updateCensus vs
      WithRecord v fs -> updateCensus v >> (let (_,vs) = unzip fs in updateCensus vs )
      ProjField v _ -> updateCensus v
      ProjIdx v _ -> updateCensus v
      List vs -> updateCensus vs
      ListCons v vs -> updateCensus v >> updateCensus vs
      Base _ -> return ()
      Lib _ _ -> return ()

instance CensusCollectible LSimpleTerm where
  updateCensus (Loc _ st) = updateCensus st

instance CensusCollectible KLambda where
  updateCensus kl = case kl of
      Unary _ lkt -> updateCensus lkt
      Nullary lkt -> updateCensus lkt

instance CensusCollectible SVal where 
  updateCensus sv = case sv of 
    KAbs kl -> updateCensus kl 
    Lit _ -> return ()

instance CensusCollectible ContDef where
  updateCensus (Cont _ lkt) = updateCensus lkt

instance CensusCollectible FunDef where
  updateCensus (Fun _ kl) = updateCensus kl

instance CensusCollectible (Located FunDef) where
  updateCensus (Loc _ fd) = updateCensus fd

instance CensusCollectible KTerm where
  updateCensus t = case t of
    LetSimple _ lst lkt -> updateCensus lst >> updateCensus lkt
    LetFun lfs lkt -> updateCensus lfs >> updateCensus lkt
    LetRet ct lkt -> updateCensus ct >> updateCensus lkt
    KontReturn x -> updateCensus x
    ApplyFun v u -> updateCensus [v,u]
    If v lk1 lk2 -> updateCensus v >> updateCensus [lk1,lk2]
    AssertElseError v lk u -> updateCensus [v,u] >> updateCensus lk
    Error v -> updateCensus v
    Halt v -> updateCensus v

instance CensusCollectible LKTerm where
  updateCensus (Loc _ kt) = updateCensus kt

getCensus :: LKTerm -> Census
getCensus lk = execState (updateCensus lk) Map.empty



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
  simpl (Fun arg kl) = simpl kl  >>= \kl' -> return $ Fun arg kl'

instance Simplifiable (Located FunDef) where
  simpl (Loc p fd) = simpl fd >>= \fd' -> return $ Loc p fd'

instance Simplifiable ContDef where
  simpl (Cont v lkt) = simplLKTerm lkt >>= return . Cont v

instance Simplifiable KLambda where
  simpl (Unary lv lk) = simplLKTerm lk >>= return . Unary lv
  simpl (Nullary lk) = simplLKTerm lk >>= return . Nullary

look :: VarName -> Opt Term
look x = do
  m <- __env_of_state <$> get
  return $ Map.findWithDefault Unknown x m

-- | Look up a Located VarName (extracts VarName from Located wrapper)
lookL :: LVarName -> Opt Term
lookL (Loc _ vn) = look vn

censusInfo :: VarName -> Opt Integer
censusInfo x = do 
  census <- __census_of_reader <$> ask 
  return $ Map.findWithDefault 0 x census


-- | Get fields from a record, taking LVarName
fields :: LVarName -> Opt LFields
fields lx = do
    w <- lookL lx
    case w of
      St (Record xs) -> return xs
      St (WithRecord ly ys) -> do
        xs <- fields ly
        return $ xs ++ ys
      _ -> return []


isRecordTerm (St (Record _)) = True
isRecordTerm (St (WithRecord _ _)) = True
isRecordTerm _ = False

recordEquiv :: LVarName -> LVarName -> Opt Bool
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
      -- Helper to substitute with LVarName (extracts VarName from Located wrapper)
      _substL (Loc _ vn) = _subst vn
      _nochange = _ret t
  in case t of
  Bin op oper1 oper2 -> do
    u <- lookL oper1
    v <- lookL oper2
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
      Basics.Eq | (isLit u && isLit v) -> _ret $ lit $ C.LBool (C.litEq (litVal u) (litVal v))
      Basics.Eq | isRecordTerm u -> do
            e <- recordEquiv oper1 oper2
            if e then _ret $ __trueLit
                 else _nochange
      Basics.Neq | isLit u && isLit v -> _ret $ lit $ C.LBool (C.litNeq (litVal u) (litVal v))

      _ -> case (u, v) of
              (St (ValSimpleTerm (Lit (C.LNumeric (NumInt n1)))),
               St (ValSimpleTerm (Lit (C.LNumeric (NumInt n2))))) ->
                    let ii f = _ret $ lit (C.LNumeric (NumInt (f n1 n2)))
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
    v <- lookL operand
    -- TODO should write out all cases
    case (op,v) of
        (Basics.IsTuple, St (Tuple _ _))          -> _ret __trueLit
        (Basics.IsTuple, St (Record _))         -> _ret __falseLit
        (Basics.IsTuple, St (WithRecord _ _))   -> _ret __falseLit
        (Basics.IsTuple, St (List _))           -> _ret __falseLit
        (Basics.IsTuple, St (ListCons _ _))     -> _ret __falseLit
        (Basics.IsTuple, St (ValSimpleTerm _))  -> _ret __falseLit


        (Basics.IsRecord, St (Record _))        -> _ret __trueLit
        (Basics.IsRecord, St (WithRecord _ _))  -> _ret __trueLit
        (Basics.IsRecord, St (Tuple _ _))         -> _ret __falseLit
        (Basics.IsRecord, St (List _))          -> _ret __falseLit
        (Basics.IsRecord, St (ListCons _ _))    -> _ret __falseLit
        (Basics.IsRecord, St (ValSimpleTerm _)) -> _ret __falseLit


        (Basics.IsList, St (List _))          -> _ret __trueLit
        (Basics.IsList, St (ListCons _ _))    -> _ret __trueLit
        (Basics.IsList, St (Record _))        -> _ret __falseLit
        (Basics.IsList, St (WithRecord _ _))  -> _ret __falseLit
        (Basics.IsList, St (Tuple _ _))         -> _ret __falseLit
        (Basics.IsList, St (ValSimpleTerm _)) -> _ret __falseLit

        -- Not: constant folding
        (Basics.Not, St (ValSimpleTerm (Lit (C.LBool b)))) ->
            _ret $ lit (C.LBool (Prelude.not b))

        -- Not: double negation elimination (not (not x) -> x)
        (Basics.Not, St (Un Basics.Not innerVar)) ->
            _substL innerVar

        -- Not: negated comparisons
        (Basics.Not, St (Bin Basics.Eq v1 v2))  -> _ret $ Bin Basics.Neq v1 v2
        (Basics.Not, St (Bin Basics.Neq v1 v2)) -> _ret $ Bin Basics.Eq v1 v2
        (Basics.Not, St (Bin Basics.Lt v1 v2))  -> _ret $ Bin Basics.Ge v1 v2
        (Basics.Not, St (Bin Basics.Le v1 v2))  -> _ret $ Bin Basics.Gt v1 v2
        (Basics.Not, St (Bin Basics.Gt v1 v2))  -> _ret $ Bin Basics.Le v1 v2
        (Basics.Not, St (Bin Basics.Ge v1 v2))  -> _ret $ Bin Basics.Lt v1 v2

        (Basics.TupleLength, St (Tuple xs _)) ->
            _ret $ lit (C.LNumeric (NumInt (fromIntegral (length xs))))
        -- 2023-08 Revision: Added this case
        (Basics.ListLength, St (List xs)) ->
            _ret $ lit (C.LNumeric (NumInt (fromIntegral (length xs))))



        _ -> _nochange
  ProjField x s ->  do
    fs <- fields x
    case lookupLast s fs of
      Just y -> _substL y
      Nothing -> _nochange
  ProjIdx x idx -> do
    t' <- lookL x
    case t' of
      St (Tuple vs _) | fromIntegral (length vs) > idx ->
        _substL (vs !! fromIntegral idx)
      _ -> _nochange


  ValSimpleTerm (KAbs klam) -> do
        klam' <- withResetRetState $ simpl klam
        _ret $ ValSimpleTerm (KAbs klam')
  _ -> _nochange

  where
    lit l = ValSimpleTerm (Lit l)
    isLit (St (ValSimpleTerm (Lit _))) = True
    isLit _ = False
    litVal (St (ValSimpleTerm (Lit (C.LNumeric n)))) = (C.LNumeric n)
    litVal (St (ValSimpleTerm (Lit x))) = x
    litVal _ = internalError "incorrect application of litVal"
    __trueLit = lit (C.LBool True)
    __falseLit = lit (C.LBool False)


withResetRetState = local (\r -> r {__rewrite_ret_of_reader = Nothing})
withRetState st = local (\r -> r {__rewrite_ret_of_reader = Just st})

failFree :: SimpleTerm -> Bool -- 2021-05-19; AA; hack
failFree st = case st of
  Bin op _ _ ->  op `elem` [Basics.Eq, Basics.Neq] -- Equality comparisons are safe (return boolean)
  Un _ _ -> False  -- Unary operations can fail (e.g., head on empty list, arithmetic on non-numbers)
  ValSimpleTerm _ -> True
  Tuple _ _ -> True
  Record _ -> True
  WithRecord _ _ -> True
  ProjField _ _ -> False  -- Field projection can fail if field doesn't exist
  ProjIdx _ _ -> False    -- Index projection can fail if index out of bounds
  List _ -> True
  ListCons _ _ -> False   -- List cons can fail if second arg is not a list
  Base _ -> False         -- Base function calls can have side effects or fail
  Lib _ _ -> False        -- Library function calls can have side effects or fail

-- Helper to unwrap LKTerm for simpl
simplLKTerm :: LKTerm -> Opt LKTerm
simplLKTerm (Loc p k) = do
    k' <- simplKTerm k
    return $ Loc p k'

-- The main simplification for KTerm
simplKTerm :: KTerm -> Opt KTerm
simplKTerm k = do
    --s <- state_info
    -- trace ("simpl-kterm\n" ++ (s) ++ "\n" ++ "~~~\n" ++(show k)++ ("\n----"))  $
    case k of
      LetSimple x (Loc stp st) lkt -> do
        _cse <- __cse_map_of_reader <$> ask
        case Map.lookup st _cse of
          Just w -> simplLKTerm (apply (Subst (Map.singleton x w)) lkt) >>= return . unLoc
          Nothing -> do
            x_uses <- censusInfo x
            case (x_uses, st) of
              (0, _) | failFree st  -> simplLKTerm lkt >>= return . unLoc
              (1, ValSimpleTerm (KAbs klambda@(Unary _ _)))
                | isApplied x lkt ->  do
                      bindenv x (St st)
                      simplLKTerm lkt >>= return . unLoc  -- remove the let-declaration
                                        -- expecting the substitution down the
                                        -- road in the application case
                                        -- 2021-05-17; AA
              _  -> do
                w <- simplifySimpleTerm st
                case w of
                  ResultSimplified st' -> do
                      bindenv x (St st')
                      lkt' <- local (\r -> r { __cse_map_of_reader = Map.insert st' x _cse } ) (simplLKTerm lkt)
                      return $ LetSimple x (Loc stp st') lkt'
                  ResultSubst w ->
                      simplLKTerm (apply (Subst (Map.singleton x w)) lkt) >>= return . unLoc
      LetFun lfdefs lkt -> do
        -- binddef fdefs
        lfdefs' <- withResetRetState $ simpl lfdefs
        lkt' <- simplLKTerm lkt
        return $ LetFun lfdefs' lkt'
      LetRet ret lkt -> do
        ret_now <- __rewrite_ret_of_reader <$> ask
        ret' <- simpl ret
        if hasUniqueReturn lkt
          then withRetState ret' (simplLKTerm lkt) >>= return . unLoc
          else do
            lkt' <- withResetRetState (simplLKTerm lkt)
            return $ LetRet ret' lkt'
      KontReturn x -> do
        ret <- __rewrite_ret_of_reader <$> ask
        case ret of
          Nothing -> return $ KontReturn x
          Just (Cont y lkt) -> return $ unLoc $ apply (Subst (Map.singleton y x)) lkt
      ApplyFun x y -> do
        x_uses <- censusInfo x
        case x_uses of
          1 -> do v <- look x
                  case v of
                    (St (ValSimpleTerm (KAbs (Unary (Loc _ arg) lbody)))) -> do
                      simplLKTerm (apply (Subst (Map.singleton arg y)) lbody) >>= return . unLoc
                    _ -> return k
          _ -> return k
      If x lk1 lk2 -> do
        v <- look x
        case v of
          St (ValSimpleTerm (Lit (C.LBool b))) ->
            simplLKTerm (if b then lk1 else lk2) >>= return . unLoc
          -- If-branch swap: if (not y) k1 k2 -> if y k2 k1
          St (Un Basics.Not (Loc _ innerVarName)) -> do
            lk1' <- withResetRetState $ simplLKTerm lk1
            lk2' <- withResetRetState $ simplLKTerm lk2
            return $ If innerVarName lk2' lk1'  -- swapped branches
          _ -> do
            lk1' <- withResetRetState $ simplLKTerm lk1
            lk2' <- withResetRetState $ simplLKTerm lk2
            return $ If x lk1' lk2'
      AssertElseError x lkt y -> do
        v <- look x
        case v of
          St (ValSimpleTerm (Lit (C.LBool b)))->
            simplLKTerm (if b then lkt else noLoc (Error y)) >>= return . unLoc
          _ -> do
              lk' <- simplLKTerm lkt
              return $ AssertElseError x lk' y
      Error _ -> return k
      Halt _ -> return k

instance Simplifiable KTerm where
  simpl = simplKTerm

instance Simplifiable LKTerm where
  simpl = simplLKTerm



hasUniqueReturn :: LKTerm -> Bool
hasUniqueReturn (Loc _ k) =
  case k of
    KontReturn _            -> True
    LetSimple _ _ lk'        -> hasUniqueReturn lk'
    LetFun _ lk'             -> hasUniqueReturn lk'
    ApplyFun _ _            -> False
    If _ _ _                -> False
    AssertElseError _ lk' _  -> hasUniqueReturn lk'
    Halt _                  -> True
    Error _                 -> True
    LetRet (Cont _ lk') _    -> hasUniqueReturn lk'

isApplied :: VarName -> LKTerm -> Bool
isApplied f (Loc _ k) =
  case k of
    KontReturn _ -> False
    LetSimple _  _ lk' -> isApplied f lk'
    LetFun lfdefs lk' ->
       or $ (isApplied f lk') :
            [ isApplied f lk'' | Loc _ (Fun _ kl) <- lfdefs, let lk'' = lkTermOfLambda kl]
    ApplyFun g _ -> g == f
    If _ lk1 lk2 -> isApplied f lk1 || isApplied f lk2
    AssertElseError  _ lk' _ -> isApplied f lk'
    Halt _ -> False
    Error _ -> False
    LetRet (Cont _ lk') lk'' -> isApplied f lk' || isApplied f lk''
   where lkTermOfLambda (Unary _ lk') = lk'
         lkTermOfLambda (Nullary lk') = lk'
    

iter :: LKTerm -> LKTerm
iter lkt =
      let census = getCensus lkt
          (lkt', _, _) = runRWS (simpl lkt)
                          OptReader {
                             __census_of_reader = census,
                             __rewrite_ret_of_reader = Nothing,
                             __cse_map_of_reader = Map.empty

                          }
                          OptState { __env_of_state = Map.empty
                          }
      in if lkt == lkt' then lkt
                      else -- trace ((show lkt) ++ ("\n------\n") ++ (show lkt') ++ "\n========\n")
                           iter lkt'

rewrite :: Prog -> Prog
rewrite (Prog lkterm) =
 Prog (iter lkterm)