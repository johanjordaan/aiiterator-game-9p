{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Board where

import Data.Aeson
import GHC.Generics
import System.Random
import Data.HashMap.Strict

import Bound
import Position

data Board = Board {
  bounds::Bounds,
  state::PositionMap
} deriving (Show, Eq, Generic)

instance ToJSON Board

initialBoard :: Bounds -> Board
initialBoard bounds = Board bounds empty

score :: Board -> Float
score (Board bounds state) = foldrWithKey (\k v a -> (distance k v) + a) 0 state

getPosition :: Board -> Position -> Position
getPosition (Board bounds state) p = case (Data.HashMap.Strict.lookup p state) of {
  Nothing -> p;
  Just v -> v;
}

shuffleBoard :: Board -> StdGen -> Int -> Board
shuffleBoard board stdGen 0 = board
shuffleBoard board stdGen count =
  let
    zero = take (length (bounds board)) (repeat 0)
    moves = getMoves board zero
    (index,newStdGen) = randomR (0,(length moves)-1) stdGen
    move = moves !! index
    newBoard = swap board zero move
  in shuffleBoard newBoard newStdGen (count-1)

swap :: Board -> Position -> Position -> Board
swap (Board bounds state) a b =
  let
    removeSame = filterWithKey (\k v -> k /= v)
    retVal pm = Board bounds pm
  in case (Data.HashMap.Strict.lookup a state,Data.HashMap.Strict.lookup b state) of {
    (Nothing,Nothing) -> retVal $ insert a b (insert b a state);
    (Just av,Nothing) -> retVal $ removeSame (insert a b (insert b av state));
    (Nothing,Just bv) -> retVal $ removeSame (insert a bv (insert b a state));
    (Just av,Just bv) -> retVal $ removeSame (insert a bv (insert b av state));
  }

getMoves :: Board -> Position -> Positions
getMoves (Board bounds state) p =
  let
    pu i = unit bounds i 1
    nu i = unit bounds i (-1)
    ff i a = (pu i):(nu i):a
    units = Prelude.foldr ff [] [0..(length bounds)-1]
    center = lookupDefault p p state
    unfiltered = Prelude.map (add center) units
  in Prelude.filter (withinBounds bounds) unfiltered

applyMove :: Board -> Position -> Board
applyMove board p = swap board p (uniform (bounds board) 0)
