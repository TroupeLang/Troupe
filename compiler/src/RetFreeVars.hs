{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module RetFreeVars where

import RetCPS as CPS
import qualified Core as C
import Data.Set (Set)
import qualified Data.Set as Set
import TroupePositionInfo (Located(..), unLoc)

newtype FreeVars = FreeVars (Set VarName)

class FreeNames a where
  freeVars :: a -> FreeVars


unionFreeVars :: FreeVars -> FreeVars -> FreeVars
unionFreeVars (FreeVars s) (FreeVars u) =
   FreeVars (s `Set.union` u)

emptyFreeVars = FreeVars Set.empty

-- obs: not tested; 2018-01-25 ; aa
unionMany :: [FreeVars] -> FreeVars
unionMany = foldl unionFreeVars emptyFreeVars

restrictFree x vs =
  let FreeVars (fv) = freeVars x
  in FreeVars ( fv Set.\\ Set.fromList vs )


instance FreeNames KLambda where
  freeVars (Unary (Loc _ vn) lkt) = restrictFree lkt [vn]
  freeVars (Nullary lkt) = restrictFree lkt []

instance FreeNames LKTerm where
  freeVars (Loc _ kt) = freeVars kt

instance FreeNames SVal where
  freeVars (KAbs klam) = freeVars klam
  freeVars (Lit (C.LAtom nm)) = FreeVars (Set.singleton $ VN nm)
  freeVars _ = emptyFreeVars

instance FreeNames ContDef where
  freeVars  (Cont vn lkt) = restrictFree lkt [vn]

instance FreeNames FunDef where
  freeVars (Fun fn klam) = restrictFree klam [fn]

instance FreeNames (Located FunDef) where
  freeVars (Loc _ fd) = freeVars fd

-- | Extract VarName from LVarName for free variable analysis
unLocVar :: LVarName -> VarName
unLocVar = unLoc

instance FreeNames SimpleTerm where
  -- Extract VarName from LVarName using unLoc
  freeVars (Bin _ lv1 lv2) = FreeVars (Set.fromList [unLocVar lv1, unLocVar lv2])
  freeVars (Un _ lv) = FreeVars (Set.singleton (unLocVar lv))
  freeVars (ValSimpleTerm sval) = freeVars sval
  freeVars (Tuple lvs _) = FreeVars (Set.fromList (map unLocVar lvs))
  freeVars (List lvs)  = FreeVars (Set.fromList (map unLocVar lvs))
  freeVars (ListCons lv1 lv2) = FreeVars (Set.fromList [unLocVar lv1, unLocVar lv2])
  freeVars (Base _ ) = FreeVars $ Set.empty
  freeVars (Lib _ _) = FreeVars $ Set.empty
  freeVars (Record fields) = unionMany $
      map (\(f,lx) -> let x = unLocVar lx in FreeVars (if x == VN f then Set.empty else Set.singleton x))
      fields
  freeVars (WithRecord lx fields) =
    let x = unLocVar lx
        _f = map (\(f,lx') -> let x' = unLocVar lx' in FreeVars (if x' == VN f then Set.empty else Set.singleton x')) fields
    in unionMany $ (FreeVars (Set.singleton x)): _f
  freeVars (ProjField lx _) = FreeVars (Set.singleton (unLocVar lx))
  freeVars (ProjIdx lx _) = FreeVars (Set.singleton (unLocVar lx))

instance FreeNames LSimpleTerm where
  freeVars (Loc _ st) = freeVars st

freeOfLet d vs lkt =
   (freeVars d) `unionFreeVars` (restrictFree lkt vs)

instance FreeNames KTerm where
  freeVars (Error v) = FreeVars (Set.singleton v)

  freeVars (LetSimple vn lst lkt) = freeOfLet lst [vn] lkt

  freeVars (LetRet (Cont vn lkt') lkt) = freeOfLet lkt [vn] lkt'

  freeVars (LetFun lfdefs lkt) =
     (unionMany (map freeVars lfdefs)) `unionFreeVars` (restrictFree lkt  (map fname lfdefs))
        where fname (Loc _ (Fun n _)) = n

  freeVars (KontReturn v) = FreeVars (Set.singleton v)

--   freeVars (LetRet cdef@(Cont vn _) kt) =  freeOfLet cdef [vn] kt

  freeVars (ApplyFun fn vn) = FreeVars (Set.fromList [fn, vn])

  freeVars (If vn lk1 lk2) =
     unionMany [freeVars lk1, freeVars lk2, FreeVars (Set.singleton vn)]

  freeVars (AssertElseError vn lk ve) =
     unionMany [freeVars lk, FreeVars $ Set.fromList [vn, ve] ]

  freeVars (Halt x) = FreeVars (Set.singleton x)
