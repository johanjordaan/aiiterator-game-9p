module BoundSpec where
import Test.Hspec

import System.Random
import Data.HashMap.Strict

import Position
import Bound
import Board

boundSpec = do
  describe "withinBounds" $ do
    it "should return false if out of bounds" $ do {
      (withinBounds (toBounds [(0,2),(0,2)]) [1,1,1]) `shouldBe` False;
      (withinBounds (toBounds [(0,2),(0,2)]) [3,1]) `shouldBe` False;
      (withinBounds (toBounds [(-10,2),(-10,2),(-10,3)]) [-11,1,9]) `shouldBe` False;
    }
    it "should return true if within bounds" $ do {
      (withinBounds (toBounds [(0,2),(0,2)]) [1,1]) `shouldBe` True;
      (withinBounds (toBounds [(0,2),(0,2)]) [0,0]) `shouldBe` True;
      (withinBounds (toBounds [(-10,2),(-10,2),(-10,3)]) [-9,1,2]) `shouldBe` True;
    }

  describe "unit" $ do
    it "should create a unit position" $ do {
      (unit (toBounds [(0,2),(0,2),(0,2)]) 1 1) `shouldBe` [0,1,0];
      (unit (toBounds [(0,2),(0,2),(0,2)]) 0 (-1)) `shouldBe` [-1,0,0];
    }

  describe "uniform" $ do
    it "should create a uniform position" $ do {
      (uniform (toBounds [(0,2),(0,2),(0,2)]) 0) `shouldBe` [0,0,0];
      (uniform (toBounds [(0,2),(0,2)]) (-1)) `shouldBe` [-1,-1];
      (uniform (toBounds [(0,2),(0,2),(0,4),(0,5)]) 3) `shouldBe` [3,3,3,3];
    }
