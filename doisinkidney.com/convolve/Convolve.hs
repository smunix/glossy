{-# LANGUAGE BangPatterns #-}

-- |
module Convolve where

-- | url : https://doisinkidney.com/posts/2018-02-17-single-pass-huffman.html#ref-bird_using_1984
convolve :: forall a b. [] a -> [] b -> [] (a, b)
convolve xs ys = walk xs const
  where
    walk :: [a] -> ([] (a, b) -> [] b -> [] (a, b)) -> [] (a, b)
    walk [] k = k [] ys
    walk (x' : xs') k = walk xs' (\r' (y' : ys') -> k ((x', y') : r') ys')

-- | url: https://haskelladdict.wordpress.com/tag/symbolic-convolution/
convolve' :: forall a b. [] a -> [] b -> [] (a, b)
convolve' xs ys = r
  where
    (r, _) = walk xs

    walk :: [] a -> ([] (a, b), [] b)
    walk [] = ([], ys)
    walk (x' : xs') = ((x', y') : r', ys')
      where
        (r', y' : ys') = walk xs'

-- | url: https://haskelladdict.wordpress.com/tag/symbolic-convolution/
convolve'2 :: forall a b. [] a -> [] b -> [] (a, b)
convolve'2 xs ys = r
  where
    (!r, _) = foldr (\x' (r', y' : ys') -> ((x', y') : r', ys')) ([], ys) xs
