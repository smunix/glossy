-- |
module Tree where

data Tree a where
  Leaf :: a -> Tree a
  (:*:) :: Tree a -> Tree a -> Tree a
  deriving (Eq, Show)
