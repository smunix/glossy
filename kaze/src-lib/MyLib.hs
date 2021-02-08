{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module MyLib (someFunc) where

import Codec.BMP
import Control.Arrow
import Control.Monad
import Control.Monad.Fix (fix)
import Data.Array
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Coerce (coerce)
import Data.FileEmbed
import Data.Sequence
import Graphics.Gloss
import Language.Haskell.TH.Syntax
import Lens.Micro.Platform
import qualified Lens.Micro.Platform as L
import TH

-- import TH (embed)

data DirMove where
  LeftMove :: DirMove
  RightMove :: DirMove
  deriving (Eq)

data DirHead where
  LeftHead :: DirHead
  RightHead :: DirHead
  deriving (Eq)

newtype Pos a where
  Pos :: {_pos :: Point} -> Pos a
  deriving (Eq)

makeLenses ''Pos

newtype Move a where
  Move :: {_move :: DirMove} -> Move a
  deriving (Eq)

makeLenses ''Move

newtype Head a where
  Head :: {_head :: DirHead} -> Head a
  deriving (Eq)

makeLenses ''Head

data Person where
  Person ::
    { _moving :: Move Person,
      _heading :: Head Person
    } ->
    Person

makeLenses ''Person

data Frames c where
  Frames ::
    { _framesR :: c Picture,
      _framesL :: c Picture,
      _framesTile :: Picture,
      _framesFood :: Picture
    } ->
    Frames c

makeLenses ''Frames

data Model where
  Model ::
    { _person :: Pos Person,
      _speed :: Point,
      _tiles :: Array (Int, Int) (Point, Char),
      _frames :: Frames Seq
    } ->
    Model
  deriving ()

makeLenses ''Model

render :: Model -> Picture
render model = pictures renderTiles <> renderPerson
  where
    renderTiles :: [Picture]
    renderTiles =
      model
        ^.. tiles . traversed . filtered (\(_, c) -> c /= '.')
        & traversed
        %~ \((!x, !y), c) ->
          c2p c
            & translate x y
    c2p :: Char -> Picture
    c2p = \case
      '*' -> model ^. frames . framesTile
      '%' -> model ^. frames . framesFood
      _ -> mempty

    renderPerson :: Picture
    renderPerson =
      model
        ^. frames
          . framesR
          . L._head
          & uncurry
            translate
            ( model
                ^. person
                  . pos
            )

window :: Display
window =
  InWindow
    "Play with Kayla"
    ( hasWidth @Display,
      hasHeight @Display
    )
    (0, 0)

instance HasSize Display Int where
  hasHeight = 768
  hasWidth = 1024

background :: Color
background = makeColor 0.2 0.1 0.1 1

fps :: Int
fps = 60

data Tile

class HasSize a m | a -> m where
  hasHeight :: m
  hasWidth :: m

instance HasSize Tile Float where
  hasHeight = 32
  hasWidth = 32

class IsHit a where
  isHit :: a -> a -> Bool

instance IsHit Point where
  isHit (x, y) (x', y') =
    (x -10) < x' + hasWidth @Tile
      && x' < x + 50 - 10
      && y < y' + hasHeight @Tile
      && y' < y + 54

class Inj b a | b -> a where
  inj :: a -> b

instance (Inj b a) => Inj ([] b) ([] a) where
  inj = fmap inj

instance Inj Picture B.ByteString where
  inj = bitmapOfBMP . packRGBA32ToBMP32 (hasWidth @Tile & ceiling) (hasHeight @Tile & ceiling)

instance Inj (Array (Int, Int) (Point, Char)) B.ByteString where
  inj bstr =
    bstrs'
      & chars
      & array bounds'
    where
      bstrs' :: [] B.ByteString
      bstrs' = bstr & C.lines & Prelude.reverse

      chars :: [] B.ByteString -> [] ((Int, Int), (Point, Char))
      chars = go 0 0 []
        where
          go ::
            Int ->
            Int ->
            [] ((Int, Int), (Point, Char)) ->
            [] B.ByteString ->
            [] ((Int, Int), (Point, Char))
          go !x !y !acc = \case
            bh : br -> go' x y acc (bh ^. unpackedChars) br
            _ -> acc
          go' ::
            Int ->
            Int ->
            [] ((Int, Int), (Point, Char)) ->
            [] Char ->
            [] B.ByteString ->
            [] ((Int, Int), (Point, Char))
          go' !x !y !acc cs br = case cs of
            c : css -> go' x' y (((x, y), ((xFn x, yFn y), c)) : acc) css br
            _ -> go 0 y' acc br
            where
              x', y' :: Int
              x' = x + 1
              y' = y + 1

              xFn, yFn :: Int -> Float
              xFn !i =
                fromIntegral i * hasWidth @Tile
                  - (hasWidth @Display & fromIntegral & (/ 2))
                  - hasWidth @Tile / 2
              yFn !i =
                fromIntegral i * hasHeight @Tile
                  - (hasHeight @Display & fromIntegral & (/ 2))
                  - hasHeight @Tile / 2
      w, h :: Int
      (!w, !h) =
        ( bstrs'
            ^. ix 0
            & C.length,
          bstrs'
            & Prelude.length
        )

      bounds' :: ((Int, Int), (Int, Int))
      !bounds' = ((0, 0), (w -1, h -1))

someFunc :: IO ()
someFunc = do
  putStrLn "someFunc"

  let model1 :: Model
      model1 =
        Model
          { _person = Pos (0.0, 0.0),
            _speed = (0, -6),
            _tiles = $(levelFile "level.txt") & inj,
            _frames =
              Frames
                { _framesL =
                    [ $(imageFile "left1.bmp"),
                      $(imageFile "left2.bmp"),
                      $(imageFile "left3.bmp"),
                      $(imageFile "left4.bmp"),
                      $(imageFile "left5.bmp"),
                      $(imageFile "left6.bmp")
                    ]
                      & inj
                      & fromList,
                  _framesR =
                    [ $(imageFile "right1.bmp"),
                      $(imageFile "right2.bmp"),
                      $(imageFile "right3.bmp"),
                      $(imageFile "right4.bmp"),
                      $(imageFile "right5.bmp"),
                      $(imageFile "right6.bmp")
                    ]
                      & inj
                      & fromList,
                  _framesFood =
                    $(imageFile "mushroom.bmp")
                      & inj,
                  _framesTile =
                    $(imageFile "tilem.bmp")
                      & inj
                }
          }

      model0 :: IO Model
      model0 = do
        let _person = Pos (0.0, 0.0)
            _speed = (0, -6)
            _tiles = $(levelFile "level.txt") & inj

        _framesFood <- loadBMP "kaze/assets/images/mushroom.bmp"
        _framesTile <- loadBMP "kaze/assets/images/tilem.bmp"
        _framesL <-
          [ "kaze/assets/images/mario.left.16x32.1.bmp",
            "kaze/assets/images/mario.left.16x32.2.bmp",
            "kaze/assets/images/mario.left.16x32.3.bmp",
            "kaze/assets/images/mario.left.16x32.4.bmp"
            ]
            <&> loadBMP
            & sequence
            <&> fromList

        _framesR <-
          [ "kaze/assets/images/mario.right.16x32.1.bmp",
            "kaze/assets/images/mario.right.16x32.2.bmp",
            "kaze/assets/images/mario.right.16x32.3.bmp",
            "kaze/assets/images/mario.right.16x32.4.bmp"
            ]
            <&> loadBMP
            & sequence
            <&> fromList

        let _frames = Frames {..}

        return Model {..}

  putStrLn "someFunc"
  print $(imageFile "tilem.bmp")
  putStrLn "someFunc"
  print $(levelFile "level.txt")
  putStrLn "someFunc"

  model' <- model0

  play
    window
    background
    fps
    model'
    render
    (\e w -> w)
    (\dt w -> w)
