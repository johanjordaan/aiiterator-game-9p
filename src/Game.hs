{-# LANGUAGE DeriveGeneric #-}
module Game where

import Data.List
import Control.Monad.Except
import System.Random

import Data.Aeson
import GHC.Generics

import Position
import Bound
import Board
import Action


type InvalidParameter = String
data Error = InvalidParameter String deriving (Eq, Show)

type PlayerId = String

type Moves = Int
data Player = Player PlayerId Moves deriving (Generic, Eq, Show)
type Players = [Player]

instance ToJSON Player
instance FromJSON Player


data GameState = GameState {
  seed::Int,
  players::Players,
  board::Board
} deriving (Generic, Eq, Show)

instance ToJSON GameState
instance FromJSON GameState

type EPlayerId = Either Error PlayerId
type EGameState = Either Error GameState
type EActionDefs = Either Error ActionDefs
type EActionValues = Either Error ActionValues


_initGame :: Bounds -> Int -> Int -> GameState
_initGame bounds seed numMoves =
  let
    stdGen = mkStdGen seed
    b0 = initialBoard bounds
    b = shuffleBoard b0 stdGen numMoves
  in GameState seed [] b;

initGame :: Bounds -> Int -> Int -> EGameState
initGame bounds seed numMoves =
  Right $ _initGame bounds seed numMoves

_joinGame :: PlayerId -> GameState -> GameState
_joinGame playerId (GameState seed players board) =
  GameState seed ((Player playerId 0):players) board

joinGame :: EPlayerId -> EGameState -> EGameState
joinGame ePlayerId eGameState = do {
  playerId <- ePlayerId;
  gameState <- eGameState;
  if(length (players gameState) /= 0)
    then Left $ (InvalidParameter "already at max (1) players")
    else Right $ _joinGame playerId gameState
}


_getActions :: PlayerId -> GameState -> ActionDefs
_getActions playerId (GameState seed players board) =
  let
    validMoves = getMoves board (uniform (bounds board) 0)
    validMovesStr = map show validMoves
  in [ActionDef "swap_with" [SelectStringDef "tile" validMovesStr 1 1 False]]

_validPlayer :: PlayerId -> Players -> Bool
_validPlayer playerId players = elem playerId (map (\(Player id _)->id) players)

getActions :: EPlayerId -> EGameState -> EActionDefs
getActions ePlayerId eGameState = do {
  playerId <- ePlayerId;
  gameState <- eGameState;
  if not (_validPlayer playerId (players gameState))
    then Left (InvalidParameter "invalid playerId")
    else Right $ _getActions playerId gameState
}

_applyAction :: PlayerId -> GameState -> ActionValues -> GameState
_applyAction playerId gameState actionValues =
  let
    ActionValue actionName parameters = head actionValues
    SelectStringValue parameterName values = head parameters
    GameState seed players board = gameState
    move = read (values !! 0) :: Position
  in GameState seed players (applyMove board move)

applyAction :: EPlayerId -> EGameState -> EActionValues -> EGameState
applyAction ePlayerId eGameState eActionValues = do {
  playerId <- ePlayerId;
  gameState <- eGameState;
  actionValues <- eActionValues;
  if not (_validPlayer playerId (players gameState))
    then Left (InvalidParameter "invalid playerId")
    else Right $ _applyAction playerId gameState actionValues
}
