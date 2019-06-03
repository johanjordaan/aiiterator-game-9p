module GameSpec where
import Test.Hspec

import Game

gameSpec = do
  describe "_initGame" $ do
    it "should create a new state with no players and a board of the correct type" $ do {
      playerState <- _initGame [3,3];
      (length (getPlayers playerState)) `shouldBe` 0;
      --(getZeroInd playerState) `shouldBe` 0;
    }

  describe "initGame" $ do
    it "should return an error on incorrect board dims" $ do {
      case (initGame [0,2,3]) of {
        Left e -> e `shouldBe` (InvalidParameter "invalid dim");
      }
    }

    it "should return a valid state" $ do {
      case (initGame [3,3,3]) of {
        Right playerStateIO -> do {
          playerState <- playerStateIO;
          (length (getPlayers playerState)) `shouldBe` 0;
          (length (getLocations playerState)) `shouldBe` 3*3*3;
          --(getZeroInd playerState) `shouldBe` 0;
        }
      }
    }
{-
  describe "joinGame" $ do
    it "should let a new the player join" $ do {
      let
        current = [[0,0],[0,1],[1,0],[1,1]]
        target = [[0,0],[0,1],[1,0],[1,1]]
        state = PlayerState [2,3] [] current target
      in do {
        case (joinGame "johan" state) of {
          Right playerState -> do {
            (length (getPlayers playerState)) `shouldBe` 1;
          }
        }
      }
    }

    it "should not let more than one player join" $ do {
      let
        current = [[0,0],[0,1],[1,0],[1,1]]
        target = [[0,0],[0,1],[1,0],[1,1]]
        state = PlayerState [2,2] [Player "lorraine" 0] current target
      in do {
        case (joinGame "johan" state) of {
          Left e -> e `shouldBe` (InvalidParameter "already at max (1) players");
        }
      }
    }
-}
