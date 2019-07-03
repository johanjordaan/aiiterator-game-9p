{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Server where

import Web.Scotty
import Network.Wai (Application)
import Network.HTTP.Types

import Data.Aeson
import GHC.Generics
import System.Random

import Bound
import Board

import Article
import Game

data InitReq = InitReq {
  bounds::Bounds,
  seed::Int,
  numMoves::Int
} deriving (Generic, Show, Eq)

instance ToJSON InitReq
instance FromJSON InitReq

routes :: ScottyM ()
routes = do
  get "/info" $ do
    Web.Scotty.text "Hallo"

  post "/init" $ do
    (InitReq bounds seed numMoves) <- jsonData :: ActionM InitReq
    let (Right gameState) = initGame bounds seed numMoves
    Web.Scotty.json $ gameState

  notFound $ do
   text "not found"


app :: IO Application
app = scottyApp routes

runServer :: IO ()
runServer = scotty 3000 routes
{-
  post "/join" $ do
    (Article id title text) <- jsonData :: ActionM Article -- Decode body of the POST request as an Article object
    let bounds = toBounds [(0,2),(0,2)]
    let board = initialBoard bounds
    json board

  post "/getactions" $ do
    (Article id title text) <- jsonData :: ActionM Article -- Decode body of the POST request as an Article object
    let bounds = toBounds [(0,2),(0,2)]
    let board = initialBoard bounds
    json board

  post "/applyaction" $ do
    (Article id title text) <- jsonData :: ActionM Article -- Decode body of the POST request as an Article object
    let bounds = toBounds [(0,2),(0,2)]
    let board = initialBoard bounds
    json board


-}
   -- get article (json)
