{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{-- Obs: 2018-02-16: beacuse of the RetCPS representation, we currently
have very few rewrites that actually kick-in; we should be able to
rectify them with some more work, but that's postponed for now; AA
--}


module RetRewrite(rewrite) where

-- todo: consider renaming this to CPSRewrite


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
import Control.Monad.Identity
import Data.Set (Set)
import qualified Data.Set as Set
import RetFreeVars as FreeVars
import TroupePositionInfo


-- substitution is a collection of both variable substitutions and
-- kont substitutions; 2018-01-23; AA (this is a rather awkward
-- construction; we should have better software engineering)


newtype Subst = Subst (Map VarName VarName)

class Substitutable a where
  apply :: Subst -> a -> a

idSubst :: Subst
idSubst = Subst (Map.empty)


instance Substitutable KLambda where
  apply subst@(Subst (varmap)) kl =
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
      List vs -> List (map fwd vs)
      ListCons v v' -> ListCons (fwd v) (fwd v')
      ValSimpleTerm sv -> ValSimpleTerm (apply subst sv)
      Base v -> Base v
      Lib l v -> Lib l v 
    where fwd x = Map.findWithDefault x x varmap

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


      -- LetRet k kt -> LetRet (kfwd k) (apply subst kt)

      Halt v -> Halt (vfwd v)

      KontReturn v  -> KontReturn (vfwd v)

      ApplyFun fn argn -> ApplyFun (vfwd fn) (vfwd argn)

      If v k1 k2 -> If (vfwd v) (apply subst k1) (apply subst k2)

      AssertElseError v k1 z p -> AssertElseError (vfwd v) (apply subst k1) (vfwd z) p

      Error x p -> Error (vfwd x) p

   where vfwd x = Map.findWithDefault x x varmap
         -- kfwd x = Map.findWithDefault x x kontmap


data Context -- note this is not an exhaustive set of possible contexts; 2018-01-25; AA
  = CtxtHole
  | CtxtLetSimple VarName SimpleTerm Context
  | CtxtLetCont ContDef Context
  | CtxtLetFunK [FunDef] Context
  | CtxtAssert VarName VarName PosInf Context
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


matchterm :: KTerm -> SearchPat -> Maybe (Context, KTerm)

matchterm found@(LetRet _ _) (PatLetRet)  =
  return (CtxtHole, found)

matchterm (LetRet _ _) PatReturn = Nothing  

matchterm found@(KontReturn _) (PatReturn) =
  return (CtxtHole, found)

matchterm found@(ApplyFun fn argn) (PatFunApply fn') | fn == fn' =
  return (CtxtHole, found)



matchterm (LetSimple vn st kt) searchTerm = do
  (ctxt, found) <- matchterm kt searchTerm
  return $ (CtxtLetSimple vn st ctxt, found)

matchterm (LetFun fdefs kt) searchTerm = do
  (ctxt, found) <- matchterm kt searchTerm
  return $ (CtxtLetFunK fdefs ctxt, found)

matchterm (LetRet kdef kt) searchTerm = do
  (ctxt, found) <- matchterm kt searchTerm
  return $ (CtxtLetCont kdef ctxt, found)

matchterm (AssertElseError vn kt vn' pos) searchTerm = do 
  (ctxt, found) <- matchterm kt searchTerm
  return $ (CtxtAssert vn vn' pos ctxt, found)


matchterm _ _ = Nothing




--- this is the inverse of match: allows us to reconstruct the term back
--- from the contxt and the term inside

reconstructTerm :: Context -> KTerm -> KTerm
reconstructTerm CtxtHole kt  = kt
reconstructTerm (CtxtLetSimple vn st ctxt) kt =
  LetSimple vn st (reconstructTerm ctxt kt)
reconstructTerm (CtxtLetCont kdef ctxt) kt =
  LetRet kdef (reconstructTerm ctxt kt)
reconstructTerm (CtxtLetFunK fdefs ctxt) kt =
  LetFun fdefs (reconstructTerm ctxt kt)
reconstructTerm (CtxtAssert vn vn' pos ctxt) kt = 
  AssertElseError vn (reconstructTerm ctxt kt) vn' pos


class KWalkable a b where
  walk :: (b -> Bool) -> (b -> b) -> a -> a

instance (KWalkable KTerm KTerm) where
  walk pred f kt =
    if pred kt then f kt
    else
      let w' = walk pred f
      in
       case kt of
         LetSimple vn st kt' -> LetSimple vn (walk pred f st) (w' kt')
         LetRet cdef kt'    -> LetRet  (walk pred f cdef) (w' kt')
         LetFun fdefs kt'    -> LetFun (map (walk pred f) fdefs) (w' kt')
         If v k1 k2          -> If v (w' k1) (w' k2)
         AssertElseError v k1 z p -> AssertElseError v (w' k1) z p
         -- LetRet kn kt'       -> LetRet kn (w' kt')
         -- these do not modify anything
         KontReturn v   -> KontReturn v
         Halt v -> Halt v
         ApplyFun v a1 -> ApplyFun v a1
         Error x p -> Error x p



instance (KWalkable KLambda KTerm) where
  walk pred f (Unary vn kt) =
    Unary vn (walk pred f kt)
  walk pred f (Nullary kt) =
    Nullary (walk pred f kt)


instance (KWalkable SimpleTerm KTerm) where
  walk pred f st =
    case st of
        ValSimpleTerm (KAbs klam) ->
          ValSimpleTerm (KAbs (walk pred f klam))
        _ -> st

instance KWalkable ContDef KTerm where
  walk pred f (Cont vn kt) = Cont vn (walk pred f kt)


instance KWalkable FunDef KTerm where
  walk pred f (Fun v klam) = Fun v (walk pred f klam)



--------------------------------------------------
-- free vars
--------------------------------------------------




instance FreeNames Context where
  freeVars CtxtHole = emptyFreeVars
  freeVars (CtxtLetSimple vn st ctxt) = freeOfLet st [vn] ctxt
  freeVars (CtxtLetCont cdef@(Cont vn kt') ctxt) = freeOfLet cdef [vn] ctxt
  freeVars (CtxtLetFunK fdefs ctxt) =
      (unionMany (map freeVars fdefs)) `unionFreeVars` (restrictFree ctxt  (map fname fdefs))
        where fname (Fun n _) = n
  freeVars (CtxtAssert vn1 vn2 _ ctxt) = unionMany [freeVars ctxt, FreeVars $ Set.fromList [vn1, vn2]]

-- todo: eliminate redundancy in code ; 2018-01-25 ; aa


--------------------------------------------------
-- REWRITES
--------------------------------------------------

betaContPred (LetRet _ _) = True
betaContPred _ = False


betaCont :: KTerm -> KTerm
betaCont (LetRet cdef@(Cont xn kt) kt') =
  let cdef' = walk betaContPred betaCont cdef
  in
    case matchterm kt' PatReturn of
                  Just (ctxt, KontReturn yn) ->
                       let kt'' = let subst = Subst ( Map.fromList ([(xn, yn)] ) )
                                  in reconstructTerm ctxt (apply subst kt)
                       in if retUnchanged ctxt
                          then kt''
                          else LetRet cdef' kt''
                  _ -> LetRet cdef'  (walk betaContPred betaCont kt')

betaCont _ = error "should not be called here"

--------------------------------------------------
-- Dead-Cont
--------------------------------------------------

-- deadContPred (LetRet _ _) = True
deadContPred _ = False

-- deadCont (LetRet cdef@(Cont _ kt) kt') =
--     let FreeVars (_, freeKs) = freeVars kt'
--     in if not (Set.member kn freeKs) then (walk deadContPred deadCont kt')
--        else
--            let cdef' = walk deadContPred deadCont cdef
--            in LetRet cdef' (walk deadContPred deadCont kt')


--------------------------------------------------
-- β-Fun (-Lin)
--------------------------------------------------

betaFunPred (LetFun [Fun fn (Unary vn kt')] kt) = True 
betaFunPred (LetSimple fn (ValSimpleTerm (KAbs (Unary vn kt')))  kt) = True
betaFunPred _ = False

betaFun :: KTerm -> KTerm
betaFun (LetFun [Fun fn klam@(Unary xn kt)] kt') =
  let klam' = walk betaFunPred betaFun klam
      noChange = LetFun [Fun fn klam'] (walk betaFunPred betaFun kt')
  in
     case matchterm kt' (PatFunApply fn) of
       Just (ctxt, ApplyFun _ yn) ->
          let kt'' = let subst = Subst (Map.fromList [(xn, yn)])
                     in reconstructTerm ctxt (apply subst kt)
              FreeVars ( freeVsCtxt ) = freeVars ctxt
              FreeVars ( freeVsKt ) = freeVars kt

          in if (not (Set.member fn (freeVsCtxt `Set.union` freeVsKt))) && ( fn /= yn)
             then kt''
             else noChange
       _ -> noChange


betaFun (LetSimple fn (ValSimpleTerm (KAbs klam@(Unary xn kt))) kt') = 
  let klam' = walk betaFunPred betaFun klam
      noChange = LetSimple fn (ValSimpleTerm (KAbs klam')) (walk betaFunPred betaFun kt')
  in
     case matchterm kt' (PatFunApply fn) of
       Just (ctxt, ApplyFun _ yn) ->
          let kt'' = let subst = Subst (Map.fromList [(xn, yn)])
                     in reconstructTerm ctxt (apply subst kt)
              FreeVars ( freeVsCtxt ) = freeVars ctxt
              FreeVars ( freeVsKt ) = freeVars kt

          in if (not (Set.member fn (freeVsCtxt `Set.union` freeVsKt))) && ( fn /= yn)
             then kt''
             else noChange
       _ -> noChange



betaFun _ = error "this should not be called"


--------------------------------------------------
-- putting it all together ...



rewrites = [ (betaFunPred, betaFun)
           ,(betaContPred, betaCont)
           -- ,(deadContPred, deadCont)
           ]


ktWalk :: KTerm -> KTerm
ktWalk kt = foldl (\kt' (pred, f) -> walk pred f kt') kt rewrites


ktWalkFix kt =
    let kt' = ktWalk kt
    in if kt' == kt then kt
       else ktWalkFix kt'

rewrite :: Prog -> Prog
rewrite (Prog atoms kterm) = Prog atoms (ktWalkFix kterm)
