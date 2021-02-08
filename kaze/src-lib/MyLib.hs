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
-- import Data.Sequence
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Language.Haskell.TH.Syntax
import Lens.Micro.Platform
import qualified Lens.Micro.Platform as L
import TH

-- import TH (embed)

data DirMove where
  LeftMove :: DirMove
  RightMove :: DirMove
  NoneMove :: DirMove
  deriving (Eq)

data DirHead where
  LeftHead :: DirHead
  RightHead :: DirHead
  deriving (Eq)

data Pos a where
  Pos ::
    { _pos :: Point,
      _object :: a
    } ->
    Pos a
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
      _heading :: Head Person,
      _sprite :: Int
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
      _frames :: Frames (Array Int)
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
          . ix
            ( model
                ^. person
                  . object
                  . sprite
            )
          & uncurry
            translate
            ( model
                ^. person
                  . pos
            )

keys :: Event -> Model -> Model
keys (EventKey (SpecialKey KeyLeft) Down _ _) model =
  model
    & person . object . moving . move .~ LeftMove
    & person . object . heading . MyLib.head .~ LeftHead
keys (EventKey (SpecialKey KeyRight) Down _ _) model =
  model
    & person . object . moving . move .~ RightMove
    & person . object . heading . MyLib.head .~ RightHead
keys (EventKey (SpecialKey KeySpace) Down _ _) model =
  model
    & speed
      . _2
      .~ if collides
        model
        '*'
        (pos' & _2 %~ (+ model ^. speed . _2))
        then 6
        else (-6)
  where
    pos' :: Point
    !pos' = model ^. person . pos
keys _ model =
  model
    & person
      . object
      . moving
      . move
      .~ NoneMove

update :: Float -> Model -> Model
update dt !model =
  model
    & speed %~ (updtSpeedX *** updtSpeedY)
    & person . object . sprite
      %~ ( \s -> case move' of
             NoneMove -> s
             _ -> (s + 1) `mod` 3
         )
    & person . pos %~ (updtPosY . updtPosX)
  where
    person' :: Person
    !person' = model ^. person . object

    speed' :: Point
    !speed' = model ^. speed

    speed'X :: Float
    !speed'X = speed' ^. _1

    speed'Y :: Float
    !speed'Y = speed' ^. _2

    dx' = speed'X
    dy' = speed'Y

    pos' :: Point
    !pos' = model ^. person . pos

    move' :: DirMove
    !move' = person' ^. moving . move

    updtSpeedY :: Float -> Float
    updtSpeedY vel =
      if collides model '*' (pos' & _2 +~ vel)
        then -3
        else max (vel - 0.1) (-6)

    updtSpeedX :: Float -> Float
    updtSpeedX vel = case move' of
      RightMove -> min 5 (vel + 0.5)
      LeftMove -> min 5 (vel + 0.5)
      _ -> max 0 (vel - 0.5)

    updtPosY :: Point -> Point
    updtPosY p'@(!x', !y') =
      if collides model '*' p''
        then p'
        else p''
      where
        p''@(!_, !_) = (x', y' + dy')

    updtPosX :: Point -> Point
    updtPosX p'@(!x', !y') = case move' of
      LeftMove ->
        if collides model '*' p'L
          then p'
          else p'L
      RightMove ->
        if collides model '*' p'R
          then p'
          else p'R
      NoneMove ->
        if speed'X > 0
          && not
            ( collides
                model
                '*'
                p'N
            )
          then p'N
          else p'
      where
        p'R@(!_, !_) = (x' + dx', y')
        p'L@(!_, !_) = (x' - dx', y')
        !p'N = case person' ^. heading . MyLib.head of
          LeftHead -> p'L
          RightHead -> p'R

collides :: Model -> Char -> Point -> Bool
collides model c p =
  model
    ^.. tiles
      . folded
      . filtered
        ( \(p', c') ->
            c == c' && isHit p p'
        )
      & not . null

window :: Display
window =
  InWindow
    "Play Mario with Kayla"
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
      bstrs' = bstr & C.lines & reverse

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
            & length
        )

      bounds' :: ((Int, Int), (Int, Int))
      !bounds' = ((0, 0), (w -1, h -1))

someFunc :: IO ()
someFunc = do
  putStrLn "someFunc"

  let model1 :: Model
      model1 =
        Model
          { _person =
              Pos
                (0.0, 0.0)
                Person
                  { _moving = Move RightMove,
                    _heading = Head RightHead,
                    _sprite = 0,
                    ..
                  },
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
                      & listArray (0, 5),
                  _framesR =
                    [ $(imageFile "right1.bmp"),
                      $(imageFile "right2.bmp"),
                      $(imageFile "right3.bmp"),
                      $(imageFile "right4.bmp"),
                      $(imageFile "right5.bmp"),
                      $(imageFile "right6.bmp")
                    ]
                      & inj
                      & listArray (0, 5),
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
        let _person =
              Pos
                (0.0, 0.0)
                Person
                  { _moving = Move RightMove,
                    _heading = Head RightHead,
                    _sprite = 0,
                    ..
                  }

            _speed = (1, 1)
            _tiles = $(levelFile "level.txt") & inj

        _framesFood <- loadBMP "kaze/assets/images/mushroom.bmp"
        _framesTile <- loadBMP "kaze/assets/images/tilem.bmp"
        _framesL <-
          [ "kaze/assets/images/mario.left.32x56.1.bmp",
            "kaze/assets/images/mario.left.32x56.2.bmp",
            "kaze/assets/images/mario.left.32x56.3.bmp"
            ]
            <&> loadBMP
            & sequence
            <&> listArray (0, 2)

        _framesR <-
          [ "kaze/assets/images/mario.right.32x56.1.bmp",
            "kaze/assets/images/mario.right.32x56.2.bmp",
            "kaze/assets/images/mario.right.32x56.3.bmp"
            ]
            <&> loadBMP
            & sequence
            <&> listArray (0, 2)

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
    keys
    update
