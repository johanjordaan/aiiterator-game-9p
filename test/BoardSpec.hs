module BoardSpec where
import Test.Hspec

import System.Random
import Data.HashMap.Strict

import Position
import Bound
import Board

boardSpec = do
  describe "swap" $ do
    it "swap two positions (2 new)" $ do {
      let
        b1 = initialBoard (toBounds [(0,2),(0,2)])
        Board bounds state = swap b1 [1,1] [0,0]
      in do {
        (length state) `shouldBe` 2;
        (state ! [1,1]) `shouldBe` [0,0];
        (state ! [0,0]) `shouldBe` [1,1];
      }
    }

    it "swap two positions (1 new)" $ do {
      let
        b1 = initialBoard (toBounds[(0,3),(0,3)])
        b2 = swap b1 [1,1] [0,0]
        Board bounds state = swap b2 [1,1] [2,2]
      in do {
        (length state) `shouldBe` 3;
        (state ! [0,0]) `shouldBe` [1,1];
        (state ! [1,1]) `shouldBe` [2,2];
        (state ! [2,2]) `shouldBe` [0,0];
      }
    }

    it "swap two positions (1 new)" $ do {
      let
        b1 = initialBoard (toBounds[(0,3),(0,3)])
        b2 = swap b1 [1,1] [0,0]
        Board bounds state = swap b2 [2,2] [1,1]
      in do {
        (length state) `shouldBe` 3;
        (state ! [0,0]) `shouldBe` [1,1];
        (state ! [1,1]) `shouldBe` [2,2];
        (state ! [2,2]) `shouldBe` [0,0];
      }
    }

    it "swap and the swap should yield the same starting position" $ do {
      let
        b1 = initialBoard (toBounds[(0,2),(0,2)])
        b2 = swap b1 [1,1] [0,0]
        Board bounds state = swap b2 [0,0] [1,1]
      in do {
        (length state) `shouldBe` 0;
      }
    }


  describe "score" $ do
    it "should get the score for the initialBoard" $ do {
      (score (initialBoard (toBounds[(0,3),(0,3)]))) `shouldBe` 0;
    }
    it "should get the score for the board" $ do {
      let
        b1 = initialBoard (toBounds[(0,3),(0,3)])
        b2 = swap b1 [1,1] [0,0]
      in do {
        (score b2) `shouldBe` 2*sqrt(1+1);
      }
    }


  describe "getPosition" $ do
    it "should get the correct unmoved position" $ do {
      (getPosition (initialBoard (toBounds [(0,3),(0,3)])) [0,0] ) `shouldBe` [0,0];
    }
    it "should get the correct swapped position" $ do {
      let
        b1 = initialBoard (toBounds [(0,2),(0,2)])
        b2 = swap b1 [0,0] [1,1]
      in do {
        (getPosition b2 [0,0]) `shouldBe` [1,1];
        (getPosition b2 [1,0]) `shouldBe` [1,0];
      }
    }


  describe "getMoves" $ do
    it "should get the correct set of moves" $ do {
      let
        b1 = initialBoard (toBounds [(0,3),(0,3),(0,3)])
      in do {
        (length (getMoves b1 [0,0,0])) `shouldBe` 3;
        (getMoves b1 [0,0,0]) `shouldBe` [[1,0,0],[0,1,0],[0,0,1]];
      }
    }
    it "should get the correct set of moves" $ do {
      let
        b1 = initialBoard (toBounds [(0,3),(0,3),(0,3)])
      in do {
        (length (getMoves b1 [0,0,0])) `shouldBe` 3;
        (getMoves b1 [0,0,0]) `shouldBe` [[1,0,0],[0,1,0],[0,0,1]];
      }
    }

  describe "shuffleBoard" $ do {
    it "should randomly pick move for n times" $ do {
      let
        stdGen = mkStdGen 123
        b0 = initialBoard (toBounds [(0,3),(0,3)])
        b = shuffleBoard b0 stdGen 2
      in do {
        (length (getState b) ) `shouldBe` 3;
        (show (getState b)) `shouldBe` "fromList [([1,1],[0,1]),([0,1],[0,0]),([0,0],[1,1])]";
      }
    }
  }
