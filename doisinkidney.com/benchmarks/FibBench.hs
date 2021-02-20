module Main (main) where

import Fib.Fib
import Gauge.Main

main :: IO ()
main =
  defaultMain
    [ bgroup
        "fib"
        [ bgroup
            (show i)
            [ bench "lazy" $ nf (fib @Lazy @Int) i,
              bench "dynamic" $ nf (fib @Dynamic @Int) i,
              bench "memoLazy" $ nf (fib @MemoLazy @Int) i
            ]
          | i <- [0, 5 .. 30]
        ]
    ]
