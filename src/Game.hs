module Game where
import Shuffle
import Coord
import Action
import Data.List

type InvalidParameter = String
data Error = InvalidParameter String deriving (Show, Eq)

type PlayerId = String
type Moves = Int
data Player = Player PlayerId Moves deriving (Show, Eq)
type Players = [Player]

data Location = Location {
  getCurrent::Coord,
  getTarget::Coord
} deriving Show
type Locations = [Location]

type GameState = CoordSpace
data PlayerState = PlayerState {
  getDims::Dim,
  getZeroInd::Int,
  getPlayers::Players,
  getLocations::Locations
} deriving Show

_findZeroInd locations target =
  let
    found = findIndex (\i -> (getCurrent i) == target ) locations
  in case found of {
    Just x -> x;
    Nothing -> 0;
  }

_initGame :: Dim -> IO PlayerState
_initGame d = do {
  shuffledBoard <- shuffle (consCoordSpace d);
  let
    l = zip shuffledBoard (consCoordSpace d)
    locations = map (\i -> Location (fst i) (snd i)) l
    zeroInd = _findZeroInd locations (zeroCoord d)
  in return $ PlayerState d zeroInd [] locations;
}

initGame :: Dim -> Either Error (IO PlayerState)
initGame d = if(validateDim d) then Right $ _initGame d else Left $ (InvalidParameter "invalid dim")

joinGame :: PlayerId -> PlayerState -> Either Error PlayerState
joinGame playerId (PlayerState d zeroInd players locations) =
  if(length players ==0) then Right $ PlayerState d zeroInd ((Player playerId 0):players) locations
  else Left $ (InvalidParameter "already at max (1) players")

getActions :: PlayerId -> PlayerState -> Either Error ActionDefs
getActions playerId (PlayerState d zeroInd players locations) =
  let
    currentZeroCoord = getCurrent $locations!!zeroInd
    possibleMoves = foldr (\i a-> (addToCoordInDim currentZeroCoord i 1):(addToCoordInDim currentZeroCoord i (-1)):a ) [] [0..(length d)-1]
    validMoves = filter (validateCoord d) possibleMoves
    validMoveIndex = [1,2,3]
  in Right $ [ActionDef "swap_with" [SelectIntDef "tile" validMoveIndex 1 1 False]]





--validIndexes :: Int -> [Int] -> [Int]
--validIndexes i b =
--  let l = round $ sqrt $ fromIntegral (length b)
--  in filter (\t -> t>0 && t<(l*l)) [i-1,i+1,i-l,i+l]

--selectIndexes :: [Int] -> [a] -> [a]
--selectIndexes is l = map snd (filter (\i -> elem (fst i) is) (zip [0..] l))

--_getEmptyIndex :: Int -> Board -> Int
--_getEmptyIndex a [] = a
--_getEmptyIndex a (x:xs) = case x of Empty -> a; NotEmpty _ -> _getEmptyIndex (a+1) xs

--getEmptyIndex :: Board -> Int
--getEmptyIndex board = _getEmptyIndex 0 board

--selectIndexes (validIndexes  (getEmptyIndex sb) [0..9-1]) b

--getActions :: PlayerId -> PlayerState -> Either Error ActionDefs
--getActions playerId (PlayerState players board target) =
--  if (elem playerId (map (\(Player id _)->id) players)) then Right (
--    let  options = validIndexes  (getEmptyIndex board) [0..(length board)-1]
--    in   [ActionDef "swap_with" [SelectIntDef "tile" options 1 1 False]]
--  )
--  else Left (InvalidParameter "invalid playerId")

--applyAction :: PlayerId -> PlayerState -> ActionValue -> Either Error PlayerState
--applyAction playerId (PlayerState players board target) action = undefined
