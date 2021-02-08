-- |
module TH (imageFile, levelFile) where

import Control.Monad
import Data.FileEmbed
import Data.Function
import Graphics.Gloss
import Language.Haskell.TH

embed :: FilePath -> FilePath -> Q Exp
embed fp = (makeRelativeToProject . (("assets/" <> fp <> "/") <>)) >=> embedFile

imageFile :: FilePath -> Q Exp
imageFile = embed "images"

levelFile :: FilePath -> Q Exp
levelFile = embed "levels"
