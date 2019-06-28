{-# LANGUAGE DeriveGeneric #-}
module Position where

import Data.Aeson
import GHC.Generics
import Data.HashMap.Strict

type Position = [Int]
type Positions = [Position]
type PositionMap = HashMap Position Position

distance :: Position -> Position -> Float
distance a b = sqrt $ fromIntegral $ Prelude.foldr (\(av,bv) acc -> (av-bv)*(av-bv)+acc )  0 (zip a b)

add :: Position -> Position -> Position
add a b = Prelude.map (\(aa,bb)->aa+bb) (zip a b)
