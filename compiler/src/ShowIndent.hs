module ShowIndent where

class ShowIndent a where
  showIndent :: Int -> a -> String
  
