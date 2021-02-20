-- |
module Main (main) where

import qualified Gauge.Main as G
import Taba.Convolve (catalan, catalan')

main :: IO ()
main =
  G.defaultMain
    [ G.bgroup
        "catalan"
        [ G.bgroup
            "list"
            [ G.bench "int" $ G.nf (cFn @Int 10) 1,
              G.bench "float" $ G.nf (cFn @Float 10) 1,
              G.bench "double" $ G.nf (cFn @Double 10) 1
            ]
        ]
    ]
  where
    cFn :: forall a. (Eq a, Num a, Ord a) => a -> a -> a
    cFn = catalan' @a @a @a (+) 0 (*)
