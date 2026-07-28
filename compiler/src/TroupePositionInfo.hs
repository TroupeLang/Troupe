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
import Sexp (Sexp(..), Datum(..), asName, headHint)

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

-- | A position is written as its own datum, carried by the @\@@ wrapper of the
-- located value it belongs to:
--
-- > ("examples/foo.trp" 12 3)   -- SrcPosInf
-- > (rt "some description")     -- RTGen
-- > none                        -- NoPos
--
-- 'NoPos' has no wrapper of its own (see the 'Located' instance), so the @none@
-- form appears only if something writes a position datum on its own.
instance Sexp PosInf where
  toSexp (SrcPosInf f l c) = Lst [Str f, toSexp (toInteger l), toSexp (toInteger c)]
  toSexp (RTGen s)         = Lst [Atom "rt", Str s]
  toSexp NoPos             = Atom "none"
  fromSexp (Atom "none")         = Right NoPos
  fromSexp (Lst [Atom "rt", sD]) = RTGen <$> asName sD
  fromSexp (Lst [fD, lD, cD])    = do
    f <- asName fD
    l <- fromSexp lD
    c <- fromSexp cD
    Right (SrcPosInf f (fromInteger l) (fromInteger c))
  fromSexp d = Left ("not a valid source position, got " ++ headHint d)

-- | A located value carries its position in an @\@@ wrapper:
--
-- > (@ ("examples/foo.trp" 12 3) (local "x"))
--
-- A value at 'NoPos' is written as its payload alone, so a position-erased
-- program prints exactly as it did before positions were representable, and a
-- document that omits positions entirely stays legal. Decoding a payload with
-- no wrapper fills 'NoPos'.
instance Sexp a => Sexp (Located a) where
  toSexp (Loc NoPos x) = toSexp x
  toSexp (Loc p x)     = Lst [Atom "@", toSexp p, toSexp x]
  fromSexp (Lst [Atom "@", pD, xD]) = Loc <$> fromSexp pD <*> fromSexp xD
  fromSexp d                        = noLoc <$> fromSexp d
