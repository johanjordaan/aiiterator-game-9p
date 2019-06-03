module Action where

data ParameterDef =
    SelectStringDef String [String] Int Int Bool
  | SelectIntDef String [Int] Int Int Bool
  | IntDef String Int Int
  | StringDef String Int deriving (Show)

data ActionDef = ActionDef String [ParameterDef] deriving (Show)
type ActionDefs = [ActionDef]

targetDef = SelectStringDef "target" ["orc","human"] 1 1 False
manaDef = IntDef "mana" 2 5
actionDef = ActionDef "cast" [manaDef, targetDef]

data ParameterValue =
    SelectValue String [String]
  | SelectIntValue String [Int]
  | IntValue String Int
  | StringValue String String deriving (Show)
data ActionValue = ActionValue String [ParameterValue] deriving (Show)
type ActionValues = [ActionValue]

targetValue = SelectValue "target" ["orc"]
manaValue = IntValue "mana" 2
actionValue = ActionValue "cast" [targetValue,manaValue]
