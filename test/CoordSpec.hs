module CoordSpec where
import Test.Hspec

import Coord

coordSpec = do
  describe "zeroCoord" $ do
    it "should create a zero coortdinate from the given dims" $ do {
      zeroCoord [3,3] `shouldBe` [0,0];
      zeroCoord [4,5] `shouldBe` [0,0];
      zeroCoord [3,3,2] `shouldBe` [0,0,0];
      zeroCoord [9,10,3,4] `shouldBe` [0,0,0,0];
    }
  describe "dimSize" $ do
    it "should return the total number of coordinates given the dimentions" $ do {
      dimSize [3,3,3] `shouldBe` 3*3*3;
      dimSize [2,3,3,10] `shouldBe` 2*3*3*10;
    }
  describe "consCoordSpace" $ do
    it "should return the coordinate space for the dims" $ do {
      consCoordSpace [2,2] `shouldBe` [[0,0],[0,1],[1,0],[1,1]];
    }
  describe "fromCoord" $ do
    it "should return the index for a coordinate" $ do {
      fromCoord [1,1] [2,2] `shouldBe` 3;
      fromCoord [2,1,1] [3,3,3] `shouldBe` 22;
    }
  describe "toCoord" $ do
    it "should return the coordinate for a index" $ do {
      toCoord 3 [2,2] `shouldBe` [1,1];
      toCoord 22 [3,3,3] `shouldBe` [2,1,1];
    }
  describe "validateCoord" $ do
    it "should check that a coord is vithin the dim limits" $ do {
      validateCoord [2,2,2] [2,2,2] `shouldBe` False;
      validateCoord [-1,2,0] [3,3,3] `shouldBe` False;
      validateCoord [2,1,9] [3,5,10] `shouldBe` True;
    }
  describe "addToCoordInDim" $ do
    it "should add the value to the dimention" $ do {
      addToCoordInDim [1,2,3] 1 (-2) `shouldBe` [1,0,3];
      addToCoordInDim [1,2,3] 2 19 `shouldBe` [1,2,22];
    }
  describe "validateDim" $ do
    it "should reject invalid dims" $ do {
      validateDim [2,2,2] `shouldBe` True;
      validateDim [0,2,2] `shouldBe` False;
    }
