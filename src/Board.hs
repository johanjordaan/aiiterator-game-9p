module Board where

import System.Random
import Data.HashMap

type Position = [Int]
type Positions = [Position]
type PositionMap = Map Position Position

distance :: Position -> Position -> Float
distance a b = sqrt $ fromIntegral $ foldr (\(av,bv) acc -> (av-bv)*(av-bv)+acc )  0 (zip a b)

add :: Position -> Position -> Position
add a b = Prelude.map (\(aa,bb)->aa+bb) (zip a b)

type Bound = (Int,Int)
type Bounds = [Bound]

withinBounds :: Bounds -> Position -> Bool
withinBounds bounds p =
  (length bounds == length p) &&
  (all (\((l,u),p)->p>=l&&p<=u) (zip bounds p))

unit :: Bounds -> Int -> Int -> Position
unit bounds index value =
  let
    impulse i = if i==index then value else 0
  in Prelude.map impulse [0..(length bounds)-1]

data Board = Board {
  getBounds::Bounds,
  getState::PositionMap
}

initialBoard :: Bounds -> Board
initialBoard bounds = Board bounds empty

swap :: Board -> Position -> Position -> Board
swap (Board bounds state) a b =
  let
    removeSame = filterWithKey (\k v -> k /= v)
    retVal = Board bounds
  in case (Data.HashMap.lookup a state,Data.HashMap.lookup b state) of {
    (Nothing,Nothing) -> retVal $ insert a b (insert b a state);
    (Just av,Nothing) -> retVal $ removeSame (insert a b (insert b av state));
    (Nothing,Just bv) -> retVal $ removeSame (insert a bv (insert b a state));
    (Just av,Just bv) -> retVal $ removeSame (insert a bv (insert b av state));
  }

score :: Board -> Float
score (Board bounds state) = foldWithKey (\k v a -> (distance k v) + a) 0 state

getPosition :: Board -> Position -> Position
getPosition (Board bounds state) p = case (Data.HashMap.lookup p state) of {
  Nothing -> p;
  Just v -> v;
}

getMoves :: Board -> Position -> Positions
getMoves (Board bounds state) p =
  let
    pu i = unit bounds i 1
    nu i = unit bounds i (-1)
    ff i a = (pu i):(nu i):a
    units = foldr ff [] [0..(length bounds)-1]
    center = findWithDefault p p state
    unfiltered = Prelude.map (add center) units
  in Prelude.filter (withinBounds bounds) unfiltered


shuffleBoard :: Board -> StdGen -> Int -> Board
shuffleBoard board stdGen 0 = board
shuffleBoard board stdGen count =
  let
    zero = take (length (getBounds board)) (repeat 0)
    moves = getMoves board zero
    (index,newStdGen) = randomR (0,(length moves)-1) stdGen
    move = moves !! index
    newBoard = swap board zero move
  in shuffleBoard newBoard newStdGen (count-1)
