{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}

-- |
module Fib where

import Data.Array
import Data.Function (fix)

data Mode where
  Lazy :: Mode
  Dynamic :: Mode
  MemoLazy :: Mode
  MemoDynamic :: Mode
  Taba :: Mode

class Fib (m :: Mode) a where
  fib :: a -> a

instance (Eq a, Num a, m ~ Lazy) => Fib Lazy a where
  fib 0 = 1
  fib 1 = 1
  fib n = fib @m (n -1) + fib @m (n -2)

instance (Ix a, Num a) => Fib MemoLazy a where
  fib n =
    memo
      (0, n)
      ( \r -> \case
          0 -> 1
          1 -> 1
          i -> r (i -1) + r (i -2)
      )
      n

instance (Eq a, Num a, m ~ Taba, Ix a) => Fib Dynamic a where
  fib n = f n
    where
      f 0 = 1
      f 1 = 1
      f n = cache ! (n -1) + cache ! (n -2)

      cache :: Array a a
      cache = array (0, n) [(i, f i) | i <- range (0, n)]

memo :: Ix a => (a, a) -> ((a -> b) -> a -> b) -> a -> b
memo bounds g = f
  where
    f i = arr ! i
    arr = array bounds [(i, g f i) | i <- range bounds]
