module GameSpec where
import Test.Hspec

import Game

containsOnlyOne :: (Eq a) => [a] -> a -> Bool
containsOnlyOne list item = (foldr (\i a -> if i==item then a+1 else a) 0 list) == 1

gameSpec = do
  describe "_initGame" $ do
    it "should create a new state with no players and a board of the correct type" $ do {
      playerState <- _initGame 9;
      (length (getPlayers playerState)) `shouldBe` 0;
      (containsOnlyOne (getBoard playerState) Empty) `shouldBe` True;
      (containsOnlyOne (getBoard playerState) (NotEmpty 1)) `shouldBe` True;
    }

  describe "initGame" $ do
    it "should return an error on an incorrect board size" $ do {
      case (initGame 10) of {
        Left e -> e `shouldBe` (InvalidParameter "invalid size");
      }
    }

    it "should return a valid state" $ do {
      case (initGame 9) of {
        Right playerStateIO -> do {
          playerState <- playerStateIO;
          (length (getPlayers playerState)) `shouldBe` 0;
          (containsOnlyOne (getBoard playerState) Empty) `shouldBe` True;
          (containsOnlyOne (getBoard playerState) (NotEmpty 1)) `shouldBe` True;
          (containsOnlyOne (getBoard playerState) (NotEmpty 2)) `shouldBe` True;
        }
      }
    }

  --describe "joinGame" $ do
  --  it "should let a new the player join" $ do {
  --    let state = PlayerState [] [Empty,NotEmpty 1,NotEmpty 2, NotEmpty 3] in do {
  --      case (joinGame "johan" state) of {
  --        Right playerState -> do {
  --          (length (getPlayers playerState)) `shouldBe` 1;
  --        }
  --      }
  --    }
  --  }
  --
  --  it "should not let more than one player join" $ do {
  --    let state = PlayerState [Player "lorraine" 0] [Empty,NotEmpty 1,NotEmpty 2, NotEmpty 3] in do {
  --      case (joinGame "johan" state) of {
  --        Left e -> e `shouldBe` (InvalidParameter "already at max (1) players");
  --      }
  --    }
  --  }
