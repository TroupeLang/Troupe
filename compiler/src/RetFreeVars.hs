module RetFreeVars where

import qualified Basics
import RetCPS as CPS
import qualified Core as C
import Data.List
import Data.Map.Lazy(Map)
import qualified Data.Map.Lazy as Map
import Control.Monad.Trans.Maybe
import Control.Monad.Identity
import Data.Set (Set)
import qualified Data.Set as Set

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
  freeVars (Unary vn kt) = restrictFree kt [vn]
  freeVars (Nullary  kt) = restrictFree kt []

instance FreeNames SVal where
  freeVars (KAbs klam) = freeVars klam
  freeVars (Lit (C.LAtom nm)) = FreeVars (Set.singleton $ VN nm)
  freeVars _ = emptyFreeVars

instance FreeNames ContDef where
  freeVars  (Cont vn kt) = restrictFree kt [vn]

instance FreeNames FunDef where
  freeVars (Fun fn klam) = restrictFree klam [fn]

instance FreeNames SimpleTerm where
  freeVars (Bin _ v1 v2) = FreeVars (Set.fromList [v1, v2])
  freeVars (Un _ v) = FreeVars (Set.singleton v)
  freeVars (ValSimpleTerm sval) = freeVars sval
  freeVars (Tuple vs) = FreeVars (Set.fromList vs)
  freeVars (List vs)  = FreeVars (Set.fromList vs)
  freeVars (ListCons v1 v2) = FreeVars (Set.fromList [v1, v2])
  freeVars (Base _ ) = FreeVars $ Set.empty
  freeVars (Lib _ _) = FreeVars $ Set.empty


freeOfLet d vs kt =
   (freeVars d) `unionFreeVars` (restrictFree kt vs)

instance FreeNames KTerm where
  freeVars (Error v _) = FreeVars (Set.singleton v)

  freeVars (LetSimple vn st kt) = freeOfLet st [vn] kt

  freeVars (LetRet (Cont vn kt')  kt) = freeOfLet kt [vn] kt'

  freeVars (LetFun fdefs kt) =
     (unionMany (map freeVars fdefs)) `unionFreeVars` (restrictFree kt  (map fname fdefs))
        where fname (Fun n _) = n

  freeVars (KontReturn v) = FreeVars (Set.singleton v)

--   freeVars (LetRet cdef@(Cont vn _) kt) =  freeOfLet cdef [vn] kt

  freeVars (ApplyFun fn vn) = FreeVars (Set.fromList [fn, vn])

  freeVars (If vn k1 k2) =
     unionMany [freeVars k1, freeVars k2, FreeVars (Set.singleton vn)]

  freeVars (AssertElseError vn k ve _) =
     unionMany [freeVars k, FreeVars $ Set.fromList [vn, ve] ]

  freeVars (Halt x) = FreeVars (Set.singleton x)
