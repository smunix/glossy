{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}

-- |
module Convolve where

import Control.Arrow

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

r'1 :: forall a. [] a -> [] a
r'1 xs = go xs id
  where
    go [] k = k []
    go (x : xs) k = go xs ((x :) . k)

{- catalan' : catalan numbers from -}
catalan' :: forall a n t. (Num a, Num n, Eq n) => (a -> t -> a) -> a -> (a -> a -> t) -> n -> a -> a
catalan' plus'op plus'zero mult'op n c0 = r'
  where
    (!r' : _) = cat'n n id

    cat'n :: n -> ([a] -> [a]) -> [a]
    cat'n 0 k = k [c0]
    cat'n !i k = cat'n (i -1) (k >>> (cat' &&& id) >>> uncurry (:))

    cat' :: [a] -> a
    cat' cs = walk'cats cs const
      where
        walk'cats :: [a] -> (a -> [a] -> a) -> a
        walk'cats [] k = k plus'zero cs
        walk'cats (c' : cs') k = walk'cats cs' \s (c : cs) -> k s cs `plus'op` (c `mult'op` c')

{- SPECIALIZE catalan' :: (Int -> Int -> Int) -> Int -> (Int -> Int -> Int) -> Int -> Int -> Int -}

-- catalan :: Int -> Int -> Int
catalan = catalan' @Int @Int @Int (+) 0 (*)
