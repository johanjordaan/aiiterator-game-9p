module Board where

import Data.HashMap

type Bound = (Int,Int)
type Bounds = [Bound]

type Position = [Int]
type Positions = [Position]
type PositionMap = Map Position Position

distance :: Position -> Position -> Float
distance a b = sqrt $ fromIntegral $ foldr (\(av,bv) acc -> (av-bv)*(av-bv)+acc )  0 (zip a b)

withinBounds :: Bounds -> Position -> Bool
withinBounds bounds p =
  (length bounds == length p) &&
  (all (\((l,u),p)->p>=l&&p<=u) (zip bounds p))

data Board = Board {
  getBounds :: Bounds,
  getState :: PositionMap
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
    --name = expression
  in undefined
