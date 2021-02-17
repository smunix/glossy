module MyLib (someFunc) where

import Circular.RepMin
import Tree.Tree

-- $> repMin $ (Leaf 3) :*: (Leaf 4)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
