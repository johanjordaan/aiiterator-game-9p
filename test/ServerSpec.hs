{-# LANGUAGE OverloadedStrings #-}
module ServerSpec where
import Test.Hspec
import Test.Hspec.Wai
import Data.Aeson

import Game

import Bound
import Server

serverSpec = with app $ do
  describe "GET /notfound" $ do
    it "should respond with 404" $ do
      get "/notfound" `shouldRespondWith` "not found" {matchStatus = 404}

  describe "GET /info" $ do
    it "should respond with 200" $ do
      get "/info" `shouldRespondWith` "Hallo" {matchStatus = 200}

  describe "POST /init" $ do
    it "should respond with 200" $ do {
      let
        bounds = toBounds [(0,2),(0,2)]
        seed = 10
        numMoves = 1
        target =  _initGame bounds seed numMoves
        targetBs = encode target
        initReq = InitReq bounds seed numMoves
        initReqStr = encode initReq
        m = ResponseMatcher status headers body
      in
        post "/init" initReqStr `shouldRespondWith` m
    }
