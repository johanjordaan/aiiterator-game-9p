module Action where

data ParameterDef =
    SelectStringDef String [String] Int Int Bool
  | SelectIntDef String [Int] Int Int Bool
  | IntDef String Int Int
  | StringDef String Int deriving (Show)

data ActionDef = ActionDef String [ParameterDef] deriving (Show)
type ActionDefs = [ActionDef]

data ParameterValue =
    SelectStringValue String [String]
  | SelectIntValue String [Int]
  | IntValue String Int
  | StringValue String String deriving (Show)
data ActionValue = ActionValue String [ParameterValue] deriving (Show)
type ActionValues = [ActionValue]
