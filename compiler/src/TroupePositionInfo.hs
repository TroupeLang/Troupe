{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}

module TroupePositionInfo


where 


import GHC.Generics(Generic)
import Data.Serialize (Serialize)

data PosInf = SrcPosInf String Int Int
            | RTGen String
            | NoPos
            deriving (Eq, Ord, Generic)


instance Serialize PosInf

instance Show PosInf
  where show (SrcPosInf filename row col) = filename ++ ":" ++ (show row) ++ ":" ++ (show col)
        show (RTGen s) = "RTGen<" ++ s ++ ">"
        show NoPos = ""


class GetPosInfo a where 
         posInfo :: a -> PosInf

instance GetPosInfo PosInf where 
         posInfo x = x