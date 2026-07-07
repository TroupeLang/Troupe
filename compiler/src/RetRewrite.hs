{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-- Obs: 2018-02-16: beacuse of the RetCPS representation, we currently
have very few rewrites that actually kick-in; we should be able to
rectify them with some more work, but that's postponed for now; AA
--}


module RetRewrite(rewrite) where

-- todo: consider renaming this to CPSRewrite


import InternalError (internalError)
import RetCPS as CPS
import Data.Map.Lazy(Map)
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import RetFreeVars as FreeVars
import TroupePositionInfo (Located(..), unLoc, noLoc, PosInf(..))


-- substitution is a collection of both variable substitutions and
-- kont substitutions; 2018-01-23; AA (this is a rather awkward
-- construction; we should have better software engineering)


newtype Subst = Subst (Map VarName VarName)

class Substitutable a where
  apply :: Subst -> a -> a

instance Substitutable KLambda where
  apply subst@(Subst (varmap)) kl =
    case kl of
      Unary lv@(Loc vnPos vn) lkt ->
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
      Tuple lvs -> Tuple (map fwdL lvs)
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


      -- LetRet k kt -> LetRet (kfwd k) (apply subst kt)

      Halt v -> Halt (vfwd v)

      KontReturn v -> KontReturn (vfwd v)

      ApplyFun fn argn -> ApplyFun (vfwd fn) (vfwd argn)

      If v lk1 lk2 -> If (vfwd v) (apply subst lk1) (apply subst lk2)

      AssertElseError v lk1 z -> AssertElseError (vfwd v) (apply subst lk1) (vfwd z)

      Error x -> Error (vfwd x)

   where vfwd x = Map.findWithDefault x x varmap
         -- kfwd x = Map.findWithDefault x x kontmap

instance Substitutable LKTerm where
  apply subst (Loc p kt) = Loc p (apply subst kt)


data Context -- note this is not an exhaustive set of possible contexts; 2018-01-25; AA
  = CtxtHole
  | CtxtLetSimple VarName LSimpleTerm Context
  | CtxtLetCont ContDef Context
  | CtxtLetFunK [Located FunDef] Context
  | CtxtAssert VarName VarName PosInf Context  -- Changed from ErrorPosInf to PosInf
--  | CtxtLetRet KontName Context
  deriving (Eq)

retUnchanged :: Context -> Bool
retUnchanged CtxtHole  = True
retUnchanged (CtxtLetSimple  _ _ ctxt) = retUnchanged ctxt
retUnchanged (CtxtLetCont _ _) = True
retUnchanged (CtxtLetFunK _ ctxt) =  retUnchanged ctxt
retUnchanged (CtxtAssert _ _ _ ctxt) = retUnchanged ctxt


data SearchPat = PatReturn
               | PatLetRet
               | PatFunApply VarName


matchterm :: LKTerm -> SearchPat -> Maybe (Context, LKTerm)

matchterm found@(Loc _ (LetRet _ _)) (PatLetRet)  =
  return (CtxtHole, found)

matchterm (Loc _ (LetRet _ _)) PatReturn = Nothing

matchterm found@(Loc _ (KontReturn _)) (PatReturn) =
  return (CtxtHole, found)

matchterm found@(Loc _ (ApplyFun fn argn)) (PatFunApply fn') | fn == fn' =
  return (CtxtHole, found)



matchterm (Loc _ (LetSimple vn lst lkt)) searchTerm = do
  (ctxt, found) <- matchterm lkt searchTerm
  return $ (CtxtLetSimple vn lst ctxt, found)

matchterm (Loc _ (LetFun lfdefs lkt)) searchTerm = do
  (ctxt, found) <- matchterm lkt searchTerm
  return $ (CtxtLetFunK lfdefs ctxt, found)

matchterm (Loc _ (LetRet kdef lkt)) searchTerm = do
  (ctxt, found) <- matchterm lkt searchTerm
  return $ (CtxtLetCont kdef ctxt, found)

matchterm (Loc pos (AssertElseError vn lkt vn')) searchTerm = do
  (ctxt, found) <- matchterm lkt searchTerm
  return $ (CtxtAssert vn vn' pos ctxt, found)  -- Use position from Located wrapper


matchterm _ _ = Nothing




--- this is the inverse of match: allows us to reconstruct the term back
--- from the contxt and the term inside

reconstructTerm :: Context -> LKTerm -> LKTerm
reconstructTerm CtxtHole lkt  = lkt
reconstructTerm (CtxtLetSimple vn lst ctxt) lkt =
  noLoc $ LetSimple vn lst (reconstructTerm ctxt lkt)
reconstructTerm (CtxtLetCont kdef ctxt) lkt =
  noLoc $ LetRet kdef (reconstructTerm ctxt lkt)
reconstructTerm (CtxtLetFunK lfdefs ctxt) lkt =
  noLoc $ LetFun lfdefs (reconstructTerm ctxt lkt)
reconstructTerm (CtxtAssert vn vn' pos ctxt) lkt =
  Loc pos $ AssertElseError vn (reconstructTerm ctxt lkt) vn'


class KWalkable a b where
  walk :: (b -> Bool) -> (b -> b) -> a -> a

instance (KWalkable LKTerm LKTerm) where
  walk pred f lkt =
    if pred lkt then f lkt
    else
      let w' = walk pred f
      in case unLoc lkt of
         LetSimple vn lst lkt' -> lkt `withLocOf'` LetSimple vn (walk pred f lst) (w' lkt')
         LetRet cdef lkt'   -> lkt `withLocOf'` LetRet  (walk pred f cdef) (w' lkt')
         LetFun lfdefs lkt'   -> lkt `withLocOf'` LetFun (map (walk pred f) lfdefs) (w' lkt')
         If v lk1 lk2         -> lkt `withLocOf'` If v (w' lk1) (w' lk2)
         AssertElseError v lk1 z -> lkt `withLocOf'` AssertElseError v (w' lk1) z
         -- LetRet kn kt'       -> LetRet kn (w' kt')
         -- these do not modify anything
         KontReturn v  -> lkt
         Halt v -> lkt
         ApplyFun v a1 -> lkt
         Error x -> lkt
    where
      withLocOf' (Loc p _) kt = Loc p kt


instance (KWalkable KLambda LKTerm) where
  walk pred f (Unary lv lkt) =
    Unary lv (walk pred f lkt)
  walk pred f (Nullary lkt) =
    Nullary (walk pred f lkt)


instance (KWalkable LSimpleTerm LKTerm) where
  walk pred f (Loc p st) = Loc p (walk pred f st)

instance (KWalkable SimpleTerm LKTerm) where
  walk pred f st =
    case st of
        ValSimpleTerm (KAbs klam) ->
          ValSimpleTerm (KAbs (walk pred f klam))
        _ -> st

instance KWalkable ContDef LKTerm where
  walk pred f (Cont vn lkt) = Cont vn (walk pred f lkt)


instance KWalkable FunDef LKTerm where
  walk pred f (Fun v klam) = Fun v (walk pred f klam)

instance KWalkable (Located FunDef) LKTerm where
  walk pred f (Loc p fd) = Loc p (walk pred f fd)


--------------------------------------------------
-- free vars
--------------------------------------------------




instance FreeNames Context where
  freeVars CtxtHole = emptyFreeVars
  freeVars (CtxtLetSimple vn lst ctxt) = freeOfLet lst [vn] ctxt
  freeVars (CtxtLetCont cdef@(Cont vn lkt') ctxt) = freeOfLet cdef [vn] ctxt
  freeVars (CtxtLetFunK lfdefs ctxt) =
      (unionMany (map freeVars lfdefs)) `unionFreeVars` (restrictFree ctxt  (map fname lfdefs))
        where fname (Loc _ (Fun n _)) = n
  freeVars (CtxtAssert vn1 vn2 _pos ctxt) = unionMany [freeVars ctxt, FreeVars $ Set.fromList [vn1, vn2]]

-- todo: eliminate redundancy in code ; 2018-01-25 ; aa


--------------------------------------------------
-- REWRITES
--------------------------------------------------

betaContPred (Loc _ (LetRet _ _)) = True
betaContPred _ = False


betaCont :: LKTerm -> LKTerm
betaCont lkt@(Loc p (LetRet cdef@(Cont xn lktBody) lkt')) =
  let cdef' = walk betaContPred betaCont cdef
  in
    case matchterm lkt' PatReturn of
                  Just (ctxt, Loc _ (KontReturn yn)) ->
                       let lkt'' = let subst = Subst ( Map.fromList ([(xn, yn)] ) )
                                  in reconstructTerm ctxt (apply subst lktBody)
                       in if retUnchanged ctxt
                          then lkt''
                          else Loc p $ LetRet cdef' lkt''
                  _ -> Loc p $ LetRet cdef'  (walk betaContPred betaCont lkt')

betaCont _ = internalError "betaCont: should not be called here"

--------------------------------------------------
-- β-Fun (-Lin)
--------------------------------------------------

betaFunPred (Loc _ (LetFun [Loc _ (Fun fn (Unary _ lkt'))] lkt)) = True
betaFunPred (Loc _ (LetSimple fn (Loc _ (ValSimpleTerm (KAbs (Unary _ lkt')))) lkt)) = True
betaFunPred _ = False

betaFun :: LKTerm -> LKTerm
betaFun lkt@(Loc p (LetFun [lfd@(Loc funPos (Fun fn klam@(Unary (Loc _ xn) lktBody)))] lkt')) =
  let klam' = walk betaFunPred betaFun klam
      noChange = Loc p $ LetFun [Loc funPos (Fun fn klam')] (walk betaFunPred betaFun lkt')
  in
     case matchterm lkt' (PatFunApply fn) of
       Just (ctxt, Loc _ (ApplyFun _ yn)) ->
          let lkt'' = let subst = Subst (Map.fromList [(xn, yn)])
                     in reconstructTerm ctxt (apply subst lktBody)
              FreeVars ( freeVsCtxt ) = freeVars ctxt
              FreeVars ( freeVsKt ) = freeVars lktBody

          in if (not (Set.member fn (freeVsCtxt `Set.union` freeVsKt))) && ( fn /= yn)
             then lkt''
             else noChange
       _ -> noChange


betaFun lkt@(Loc p2 (LetSimple fn (Loc p1 (ValSimpleTerm (KAbs klam@(Unary (Loc _ xn) lktBody)))) lkt')) =
  let klam' = walk betaFunPred betaFun klam
      noChange = Loc p2 $ LetSimple fn (Loc p1 (ValSimpleTerm (KAbs klam'))) (walk betaFunPred betaFun lkt')
  in
     case matchterm lkt' (PatFunApply fn) of
       Just (ctxt, Loc _ (ApplyFun _ yn)) ->
          let lkt'' = let subst = Subst (Map.fromList [(xn, yn)])
                     in reconstructTerm ctxt (apply subst lktBody)
              FreeVars ( freeVsCtxt ) = freeVars ctxt
              FreeVars ( freeVsKt ) = freeVars lktBody

          in if (not (Set.member fn (freeVsCtxt `Set.union` freeVsKt))) && ( fn /= yn)
             then lkt''
             else noChange
       _ -> noChange



betaFun _ = internalError "betaFun: this should not be called"


--------------------------------------------------
-- putting it all together ...



contextualRewrites = [ (betaFunPred, betaFun)
                     , (betaContPred, betaCont)
                     -- ,(deadContPred, deadCont)
                     ]


lktWalk :: LKTerm -> LKTerm
lktWalk lkt =
   let rewrites =
          map (\(pred, f) -> walk pred f) contextualRewrites
   in
    foldl (\t rwrt -> rwrt t) lkt rewrites


lktWalkFix lkt =
    let lkt' = lktWalk lkt
    in if lkt' == lkt then lkt
       else lktWalkFix lkt'

rewrite :: Prog -> Prog
rewrite (Prog atoms lkterm) = Prog atoms (lktWalkFix lkterm)
