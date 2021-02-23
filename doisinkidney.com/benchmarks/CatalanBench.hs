-- |
module Main (main) where

import qualified Gauge.Main as G
import Taba.Convolve (catalan, catalan', catalan'naive)

main :: IO ()
main =
  G.defaultMain
    [ G.bgroup
        "catalan"
        [ G.bgroup
            "circular"
            [ G.bench "int" $ G.nf (\c0 -> cFn @Int 10 c0) 1,
              G.bench "float" $ G.nf (\c0 -> cFn @Float 10 c0) 1,
              G.bench "double" $ G.nf (\c0 -> cFn @Double 10 c0) 1
            ],
          G.bgroup
            "naive"
            [ G.bench "int" $ G.nf (\n -> catalan'naive @Int 1 n) 10,
              G.bench "float" $ G.nf (\n -> catalan'naive @Float 1 n) 10,
              G.bench "double" $ G.nf (\n -> catalan'naive @Double 1 n) 10
            ]
        ]
    ]
  where
    cFn :: forall a. (Eq a, Num a, Ord a) => a -> a -> a
    cFn = catalan' @a @a @a (+) 0 (*)
