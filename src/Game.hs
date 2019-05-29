module Game where
import Shuffle

type InvalidParameter = String
data Error = InvalidParameter String deriving (Show, Eq)

data ParameterDef =
    SelectStringDef String [String] Int Int Bool
  | SelectIntDef String [Int] Int Int Bool
  | IntegerDef String Int Int
  | StringDef String Int

data ActionDef = ActionDef String [ParameterDef]
type ActionDefs = [ActionDef]

targetDef = SelectStringDef "target" ["orc","human"] 1 1 False
manaDef = IntegerDef "mana" 2 5
actionDef = ActionDef "cast" [manaDef, targetDef]

data ParameterValue =
    SelectValue String [String]
  | IntegerValue String Int
  | StringValue String String
data ActionValue = ActionValue String [ParameterValue]
type ActionAalues = [ActionValue]

targetValue = SelectValue "target" ["orc"]
manaValue = IntegerValue "mana" 2
actionValue = ActionValue "cast" [targetValue,manaValue]


data Position = Empty | NotEmpty Int deriving (Show, Eq)
type PlayerId = String
type Moves = Int
data Player = Player PlayerId Moves deriving (Show, Eq)

type Players = [Player]
type Board = [Position]
data PlayerState = PlayerState {
  getPlayers::Players,
  getBoard::Board
} deriving Show

_initGame :: Int -> IO PlayerState
_initGame size = do {
  shuffledBoard <- shuffle (Empty : map NotEmpty [1 .. size-1]);
  return $ PlayerState [] shuffledBoard;
}

initGame :: Int -> Either Error (IO PlayerState)
initGame size = if(size<=9) then Right $ _initGame size else Left $ (InvalidParameter "invalid size")

joinGame :: PlayerId -> PlayerState -> Either Error PlayerState
joinGame playerId (PlayerState players board) =
  if(length players ==0) then Right $ PlayerState ((Player playerId 0):players) board
  else Left $ (InvalidParameter "already at max (1) players")

validIndexes :: Int -> [Int] -> [Int]
validIndexes i b =
  let l = round $ sqrt $ fromIntegral (length b)
  in filter (\t -> t>0 && t<(l*l)) [i-1,i+1,i-l,i+l]

selectIndexes :: [Int] -> [a] -> [a]
selectIndexes is l = map snd (filter (\i -> elem (fst i) is) (zip [0..] l))

findX

getActions :: PlayerId -> PlayerState -> Either Error ActionDefs
getActions playerId (PlayerState players board) = undefined

applyAction :: PlayerId -> PlayerState -> ActionValue -> Either Error PlayerState
applyAction playerId (PlayerState players board) action = undefined
