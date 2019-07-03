{-# LANGUAGE DeriveGeneric #-}
module Bound where

import Data.Aeson
import GHC.Generics

import Position

type BoundTuple = (Int,Int)
type BoundTuples = [BoundTuple]

data Bound = Bound {
  lower::Int,
  upper::Int
} deriving (Generic, Eq, Show)
type Bounds = [Bound]

instance ToJSON Bound
instance FromJSON Bound

withinBounds :: Bounds -> Position -> Bool
withinBounds bounds p =
  (length bounds == length p) &&
  (all (\((Bound l u),p)->p>=l&&p<=u) (zip bounds p))

unit :: Bounds -> Int -> Int -> Position
unit bounds index value =
  let
    impulse i = if i==index then value else 0
  in map impulse [0..(length bounds)-1]

uniform :: Bounds -> Int -> Position
uniform bounds value = take (length bounds) (repeat value)

toBound :: BoundTuple -> Bound
toBound t = Bound (fst t) (snd t)

toBounds :: BoundTuples -> Bounds
toBounds ts = map toBound  ts
