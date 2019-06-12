module Game where
import Shuffle
import Coord
import Action
import Data.List
import Control.Monad.Except
import System.Random


type InvalidParameter = String
data Error = InvalidParameter String deriving (Show, Eq)

type PlayerId = String

type Moves = Int
data Player = Player PlayerId Moves deriving (Show, Eq)
type Players = [Player]

data Location = Location {
  getCurrent::Coord,
  getTarget::Coord
} deriving (Eq,Show)
type Locations = [Location]

type GameState = CoordSpace
data PlayerState = PlayerState {
  getDims::Dim,
  getSeed::Int,
  getPlayers::Players,
  getLocations::Locations
} deriving Show


type EPlayerId = Either Error PlayerId
type EPlayerState = Either Error PlayerState
type EActionDefs = Either Error ActionDefs

_findTargetCoord :: Locations -> Coord -> Location
_findTargetCoord locations target =
  let
    found = find (\i -> (getTarget i) == target ) locations
  in case found of {
    Just x -> x;
  }

_initGame :: Dim -> Int -> PlayerState
_initGame dim seed =
  let
    shuffledBoard = shuffle' (consCoordSpace dim) (mkStdGen seed);
    l = zip (fst shuffledBoard) (consCoordSpace dim)
    locations = map (\i -> Location (fst i) (snd i)) l
  in PlayerState dim seed [] locations;

initGame :: Dim -> Int -> EPlayerState
initGame dim seed =
  if(validateDim dim)
    then Right $ _initGame dim seed
    else Left (InvalidParameter "invalid dim")

_joinGame :: PlayerId -> PlayerState -> PlayerState
_joinGame playerId (PlayerState dim seed players locations) =
  PlayerState dim seed ((Player playerId 0):players) locations

joinGame :: EPlayerId -> EPlayerState -> EPlayerState
joinGame ePlayerId ePlayerState = do {
  playerId <- ePlayerId;
  playerState <- ePlayerState;
  if(length (getPlayers playerState) /= 0)
    then Left $ (InvalidParameter "already at max (1) players")
    else Right $ _joinGame playerId playerState
}

_getActions :: PlayerId -> PlayerState -> ActionDefs
_getActions playerId (PlayerState dim seed players locations) =
  let
    currentZeroCoord = getCurrent $ _findTargetCoord locations (zeroCoord dim)
    possibleMoves = foldr (\i a-> (addToCoordInDim currentZeroCoord i 1):(addToCoordInDim currentZeroCoord i (-1)):a ) [] [0..(length dim)-1]
    validMoves = filter (validateCoord dim) possibleMoves
    validMoveIndex = map (fromCoord dim) validMoves
  in [ActionDef "swap_with" [SelectIntDef "tile" validMoveIndex 1 1 False]]


_validPlayer :: PlayerId -> Players -> Bool
_validPlayer playerId players = elem playerId (map (\(Player id _)->id) players)

getActions :: EPlayerId -> EPlayerState -> EActionDefs
getActions ePlayerId ePlayerState = do {
  playerId <- ePlayerId;
  playerState <- ePlayerState;
  if not (_validPlayer playerId (getPlayers playerState))
    then Left (InvalidParameter "invalid playerId")
    else Right $ _getActions playerId playerState
}

_swapLocations :: Locations -> Location -> Location -> Locations
_swapLocations l x y =
  let
    nx = Location (getCurrent y) (getTarget x)
    ny = Location (getCurrent x) (getTarget y)
    nl = filter (\t -> (getTarget t) /= (getTarget x) && (getTarget t) /= (getTarget y)  ) l
  in nx:ny:nl

_applyAction :: PlayerId -> PlayerState -> ActionValues -> PlayerState
_applyAction playerId playerState actionValues =
  let
    ActionValue actionName parameters = head actionValues
    SelectIntValue parameterName values = head parameters
    PlayerState dim seed players locations = playerState
    selectedCoord = toCoord dim (values !! 0)
    selectedLocation = _findTargetCoord locations selectedCoord
    zeroLocation = _findTargetCoord locations (zeroCoord dim)
  in PlayerState dim seed players (_swapLocations locations zeroLocation selectedLocation)

--applyAction :: PlayerId -> PlayerState -> ActionValue -> Either Error PlayerState
--applyAction playerId (PlayerState players board target) action = undefined
