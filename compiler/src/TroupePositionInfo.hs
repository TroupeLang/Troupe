{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module TroupePositionInfo
  ( PosInf(..)
  , GetPosInfo(..)
  , Located(..)
  , getLoc
  , unLoc
  , noLoc
  , atLoc
  , mapLoc
  , withLocOf
  )

where 


import GHC.Generics(Generic)
import Data.Serialize (Serialize)
import Sexp (ToSexp(..), FromSexp(..))

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


-- | A value annotated with source position information.
-- This wrapper separates position tracking from AST node content,
-- following the GHC approach to source locations.
-- Note: Uses 'Loc' instead of 'L' to avoid conflict with Lexer.L
data Located a = Loc !PosInf a
  deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

-- | Ord instance compares content only, ignoring position.
-- This is useful for CSE maps and other structural comparisons.
instance Ord a => Ord (Located a) where
  compare (Loc _ x) (Loc _ y) = compare x y

instance Serialize a => Serialize (Located a)

instance GetPosInfo (Located a) where
  posInfo = getLoc

-- | Extract position from a located value
getLoc :: Located a -> PosInf
getLoc (Loc p _) = p

-- | Extract content from a located value
unLoc :: Located a -> a
unLoc (Loc _ x) = x

-- | Wrap a value with no position information
noLoc :: a -> Located a
noLoc = Loc NoPos

-- | Wrap a value with a specific position
atLoc :: PosInf -> a -> Located a
atLoc = Loc

-- | Map over the content of a located value (same as fmap, but explicit)
mapLoc :: (a -> b) -> Located a -> Located b
mapLoc = fmap

-- | Combine two located values, keeping the position of the first
withLocOf :: Located a -> b -> Located b
withLocOf (Loc p _) x = Loc p x

------------------------------------------------------------
-- s-expression serialization (see "Sexp")
------------------------------------------------------------

-- | Positions are not represented in troupe-ir-sexp version 1: encoding a
-- located value writes its payload, and decoding fills 'NoPos'. The round-trip
-- law is therefore stated over position-erased ASTs
-- (@compiler/docs/spec-troupe-ir-sexp.md@).
instance ToSexp a => ToSexp (Located a) where
  toSexp = toSexp . unLoc

instance FromSexp a => FromSexp (Located a) where
  fromSexp d = noLoc <$> fromSexp d
