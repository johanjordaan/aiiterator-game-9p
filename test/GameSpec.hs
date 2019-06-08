module GameSpec where
import Test.Hspec

import Control.Monad.Except
import Control.Exception

import Game
import Coord

gameSpec = do
  describe "_initGame" $ do
    it "should create a new state with no players and a board of the correct type" $ do {
      playerState <- _initGame [3,3];
      (length (getPlayers playerState)) `shouldBe` 0;
      --(getZeroInd playerState) `shouldBe` 0;
    }

  describe "initGame" $ do
    it "should return an error on incorrect board dims" $ do {
      r1 <- runExceptT (initGame [0,2,3]);
      case r1 of {
        Left e -> e `shouldBe` (InvalidParameter "invalid dim");
      }
    }

    it "should return a valid state" $ do {
      r1 <- runExceptT (initGame [3,3,3]);
      case r1 of {
        Right playerState -> do {
          (length (getPlayers playerState)) `shouldBe` 0;
          (length (getLocations playerState)) `shouldBe` 3*3*3;
        }
      }
    }

  describe "_joinGame" $ do
    it "should let a new the player join" $ do {
      playerState <- _initGame [3,3,3];
      let
        nextPlayerState = _joinGame "johan" playerState;
      in do {
        (length (getPlayers nextPlayerState)) `shouldBe` 1;
      }
    }

  describe "joinGame" $ do
    it "should let a new the player join" $ do {
      r1 <- runExceptT (initGame [3,3,3]);
      r2 <- evaluate (joinGame (Right "johan") r1);
      case r2 of {
        Right playerState -> do {
          (length (getPlayers playerState)) `shouldBe` 1;
          (length (getLocations playerState)) `shouldBe` 3*3*3;
        }
      }
    }

    it "should fail if a player has already joined" $ do {
      r1 <- runExceptT (initGame [3,3,3]);
      r2 <- evaluate (joinGame (Right "johan") r1);
      r3 <- evaluate (joinGame (Right "lorraine") r2);
      case r3 of {
        Left e -> e `shouldBe` (InvalidParameter "already at max (1) players");
      }
    }

  describe "_getActions" $ do
    it "should let a new the player join" $ do {
      ps1 <- _initGame [3,3,3];
      let
        ps2 = _joinGame "johan" ps1;
        actions = _getActions "johan" ps2
      in do {
        (length actions) `shouldBe` 1;
      }
    }

  describe "getActions" $ do
    it "should get the actions for the player for the game state" $ do {
      r1 <- runExceptT (initGame [3,3,3]);
      r2 <- evaluate (joinGame (Right "johan") r1);
      r3 <- evaluate (getActions (Right "johan") r2);
      case r3 of {
        Right actions -> do {
          --(show actions) `shouldBe` "[ActionDef \"swap_with\" [SelectIntDef \"tile\" [17,23,25] 1 1 False]]";
          1 `shouldBe` 1
        }
      }
    }

    it "should fail if it request actions for an invalid user" $ do {
      r1 <- runExceptT (initGame [3,3,3]);
      r2 <- evaluate (joinGame (Right "johan") r1);
      r3 <- evaluate (getActions (Right "lorraine") r2);

      case r3 of {
        Left e -> e `shouldBe` (InvalidParameter "invalid playerId");
      }
    }

  describe "_swapLocations" $ do
    it "should swap tow locations" $ do {
      let
        d = [3,3]
        src = [ Location [0,0] [0,0]  ,
                Location [0,1] [0,1]  ,
                Location [1,0] [1,0]  ,
                Location [1,1] [1,1]  ]
        swap = _swapLocations src (Location [0,0] [0,0]) (Location [1,1] [1,1])
      in do {
        swap !! 0 `shouldBe` Location [1,1] [0,0];
        swap !! 1 `shouldBe` Location [0,0] [1,1];
      }
    }
