-- |
module Main (main) where

import qualified Gauge.Main as G
import Taba.Convolve (catalan, catalan', catalan'memoization, catalan'naive)

main :: IO ()
main =
  G.defaultMain
    [ G.bgroup
        "catalan"
        [ G.bgroup
            "circular"
            [ G.bench "int" $ G.nf (\c0 -> cFn @Int (fromIntegral count) c0) (fromIntegral 1),
              G.bench "float" $ G.nf (\c0 -> cFn @Float (fromIntegral count) c0) (fromIntegral 1),
              G.bench "double" $ G.nf (\c0 -> cFn @Double (fromIntegral count) c0) (fromIntegral 1)
            ],
          G.bgroup
            "naive"
            [ G.bench "int" $ G.nf (\n -> catalan'naive @Int n) (fromIntegral count'),
              G.bench "float" $ G.nf (\n -> catalan'naive @Float n) (fromIntegral count'),
              G.bench "double" $ G.nf (\n -> catalan'naive @Double n) (fromIntegral count')
            ],
          G.bgroup
            "memoization"
            [ G.bench "int" $ G.nf (\n -> catalan'memoization @Int n) (fromIntegral count)
            -- G.bench "float" $ G.nf (\n -> catalan'memoization @Float n) (fromIntegral count),
            -- G.bench "double" $ G.nf (\n -> catalan'memoization @Double n) (fromIntegral count)
            ]
        ]
    ]
  where
    cFn :: forall a. (Eq a, Num a, Ord a) => a -> a -> a
    cFn = catalan' @a @a @a (+) 0 (*)
    count = 100
    count' = 10
