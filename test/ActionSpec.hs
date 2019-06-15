module ActionSpec where
import Test.Hspec

import Action

actionSpec = do
  describe "actionDef" $ do
    it "should create a valid actionDef" $ do {
      let
        targetDef = SelectStringDef "target" ["orc","human"] 1 1 False
        manaDef = IntDef "mana" 2 5
        actionDef = ActionDef "cast" [manaDef, targetDef]
      in 1 `shouldBe` 1
    }
  describe "actionValue" $ do
    it "should create a valid actionValue" $ do {
      let
        targetValue = SelectStringValue "target" ["orc"]
        manaValue = IntValue "mana" 2
        actionValue = ActionValue "cast" [targetValue,manaValue]
      in 1 `shouldBe` 1
    }
