module GameSpec where
import Test.Hspec

import Control.Monad.Except
import Control.Exception

import Position
import Bound
import Game
import Board
import Action

gameSpec = do
  describe "_initGame" $ do
    it "should create a new state with no players and a board of the correct type" $ do {
      let
        gameState = _initGame (toBounds [(0,3),(0,3)]) 123 3;
      in do {
        (length (getPlayers gameState)) `shouldBe` 0;
      }
    }

  describe "initGame" $ do
    it "should return a valid state" $ do {
      let
        r1 = initGame (toBounds [(0,3),(0,3)]) 123 3;
      in case r1 of {
        Right gameState -> do {
          (length (getPlayers gameState)) `shouldBe` 0;
          (getBounds (getBoard gameState)) `shouldBe` (toBounds [(0,3),(0,3)]);
        }
      }
    }

  describe "_joinGame" $ do
    it "should let a new the player join" $ do {
      let
        gameState = _initGame (toBounds [(0,3),(0,3)]) 123 3;
        nextGameState = _joinGame "johan" gameState;
      in do {
        (length (getPlayers nextGameState)) `shouldBe` 1;
      }
    }

  describe "joinGame" $ do
    it "should let a new the player join" $ do {
      let
        r1 = initGame (toBounds [(0,3),(0,3)]) 123 3;
        r2 = joinGame (Right "johan") r1;
      in case r2 of {
        Right gameState -> do {
          (length (getPlayers gameState)) `shouldBe` 1;
        }
      }
    }

    it "should fail if a player has already joined" $ do {
      let
        r1 = initGame (toBounds [(0,3),(0,3)]) 123 3;
        r2 = joinGame (Right "johan") r1;
        r3 = joinGame (Right "lorraine") r2;
      in case r3 of {
        Left e -> e `shouldBe` (InvalidParameter "already at max (1) players");
      }
    }


  describe "_getActions" $ do
    it "should get the actions for the player given the state" $ do {
      let
        ps1 = _initGame (toBounds [(0,3),(0,3)]) 123 3;
        ps2 = _joinGame "johan" ps1;
        actions = _getActions "johan" ps2;
      in do {
        (show actions) `shouldBe` "[ActionDef \"swap_with\" [SelectStringDef \"tile\" [\"[2,0]\",\"[0,0]\",\"[1,1]\"] 1 1 False]]";
      }
    }
  describe "getActions" $ do
    it "should get the actions for the player for the game state" $ do {
      let
        r1 = initGame (toBounds [(0,3),(0,3)]) 123 3;
        r2 = joinGame (Right "johan") r1;
        r3 = getActions (Right "johan") r2;
      in case r3 of {
        Right actions -> do {
          (show actions) `shouldBe` "[ActionDef \"swap_with\" [SelectStringDef \"tile\" [\"[2,0]\",\"[0,0]\",\"[1,1]\"] 1 1 False]]";
          1 `shouldBe` 1
        }
      }
    }

    it "should fail if it request actions for an invalid user" $ do {
      let
        r1 = initGame (toBounds [(0,3),(0,3)]) 123 3;
        r2 = joinGame (Right "johan") r1;
        r3 = getActions (Right "lorraine") r2;
      in case r3 of {
        Left e -> e `shouldBe` (InvalidParameter "invalid playerId");
      }
    }

  describe "_applyAction" $ do
    it "should apply the action to the board" $ do {
      let
        r1 = _initGame (toBounds [(0,3),(0,3)]) 123 3;
        r2 = _joinGame "johan" r1;
        actions = _getActions "johan" r2;
        tileValue = SelectStringValue "tile" ["[1,1,2]"]
        actionValues = [ActionValue "swap_with" [tileValue]]
        r3 = _applyAction "johan" r2 actionValues
      in do {
        r2 `shouldNotBe` r3;
      }
    }


  describe "applyAction" $ do
    it "should apply the action to the board" $ do {
      let
        r1 = initGame (toBounds [(0,3),(0,3)]) 123 3;
        r2 = joinGame (Right "johan") r1;
        actions = getActions (Right "johan") r2;
        tileValue = SelectStringValue "tile" ["[1,1,2]"]
        actionValues = [ActionValue "swap_with" [tileValue]]
        r3 = applyAction (Right "johan") r2 (Right actionValues)
      in do {
        r2 `shouldNotBe` r3;
      }
    }

    it "should fail if it request actions for an invalid user" $ do {
      let
        r1 = initGame (toBounds [(0,3),(0,3)]) 123 3;
        r2 = joinGame (Right "johan") r1;
        actions = getActions (Right "johan") r2;
        tileValue = SelectStringValue "tile" ["[1,1,2]"]
        actionValues = [ActionValue "swap_with" [tileValue]]
        r3 = applyAction (Right "lorraine") r2 (Right actionValues)
      in case r3 of {
        Left e -> e `shouldBe` (InvalidParameter "invalid playerId");
      }
    }
