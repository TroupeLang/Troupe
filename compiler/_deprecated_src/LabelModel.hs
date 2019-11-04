module LabelModel
where

data Label
  = Secret
  | Public
  deriving (Eq, Show)

instance Ord Label where
  Secret <= Public = False
  _ <= _ = True

bottom :: Label
bottom = Public

top = Secret

lub :: Label -> Label -> Label
lub = max
