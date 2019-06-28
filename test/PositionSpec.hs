module PositionSpec where
import Test.Hspec

import System.Random
import Data.HashMap.Strict

import Position
import Bound
import Board

positionSpec = do
  describe "add" $ do
    it "should return the elemnt wise addition of the two positions" $ do {
      (add [1,2,3] [1,-1,3]) `shouldBe` [2,1,6];
    }

  describe "distance" $ do
    it "should distnace betrween twom positions" $ do {
      (distance [1,2,3] [1,-1,3]) `shouldBe` sqrt(3*3)
    }
