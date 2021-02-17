{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
module RepMin where

import Tree.Tree

repMin :: forall a. (Ord a) => Tree a -> Tree a
repMin xs = xs'
  where
    (m, xs') = go xs
    go :: Tree a -> (a, Tree a)
    go (Leaf x) = (x, Leaf m)
    go (as :*: bs) = (min a' b', as' :*: bs')
      where
        (a', as') = go as
        (b', bs') = go bs
