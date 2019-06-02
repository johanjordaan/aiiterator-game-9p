module Game where
import Shuffle

type InvalidParameter = String
data Error = InvalidParameter String deriving (Show, Eq)

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

---------------------------------------
--data Dimentions = Dimentions [Int]
--data Coordinate = Coordinate [Int]
--type Current = Coordinate
--type Target = Coordinate
--data LocationType = EmptyLocation | NonEmptyLocation
--data Location = Location LocationType Current Target
--type Locations = [Locations]

--d2 = Dimentions [3,3]

--_initSpace ::  Locations -> Dimentions -> Locations
--_initSpace a [] = a
--_initSpace a (d:ds) = map (\i -> _initSpace a ds) [0..d-1]

--initSpace = _initSpace []



--initSquare :: Int -> Square
--initSquare value x y=
--  if value = 0 then Square EmptySquare Coordinate 0 0
--  else Square EmptySquare Coordinate 0 0

--initSquares :: Int -> Squares
--initSquares size = map initSquare [0..size-1]



---------------------------------------

data Position = Empty | NotEmpty Int deriving (Show, Eq)
type PlayerId = String
type Moves = Int
data Player = Player PlayerId Moves deriving (Show, Eq)

type Players = [Player]
type Board = [Position]
data PlayerState = PlayerState {
  getPlayers::Players,
  getBoard::Board,
  getTargetBoard::Board
} deriving Show

startBoard :: Int -> Board
startBoard size = (Empty : map NotEmpty [1 .. size-1])

_initGame :: Int -> IO PlayerState
_initGame size = do {
  shuffledBoard <- shuffle (startBoard size);
  return $ PlayerState [] shuffledBoard (startBoard size);
}

initGame :: Int -> Either Error (IO PlayerState)
initGame size = if(size<=9) then Right $ _initGame size else Left $ (InvalidParameter "invalid size")

joinGame :: PlayerId -> PlayerState -> Either Error PlayerState
joinGame playerId (PlayerState players board target) =
  if(length players ==0) then Right $ PlayerState ((Player playerId 0):players) board target
  else Left $ (InvalidParameter "already at max (1) players")

validIndexes :: Int -> [Int] -> [Int]
validIndexes i b =
  let l = round $ sqrt $ fromIntegral (length b)
  in filter (\t -> t>0 && t<(l*l)) [i-1,i+1,i-l,i+l]

selectIndexes :: [Int] -> [a] -> [a]
selectIndexes is l = map snd (filter (\i -> elem (fst i) is) (zip [0..] l))

_getEmptyIndex :: Int -> Board -> Int
_getEmptyIndex a [] = a
_getEmptyIndex a (x:xs) = case x of Empty -> a; NotEmpty _ -> _getEmptyIndex (a+1) xs

getEmptyIndex :: Board -> Int
getEmptyIndex board = _getEmptyIndex 0 board

--selectIndexes (validIndexes  (getEmptyIndex sb) [0..9-1]) b

getActions :: PlayerId -> PlayerState -> Either Error ActionDefs
getActions playerId (PlayerState players board target) =
  if (elem playerId (map (\(Player id _)->id) players)) then Right (
    let  options = validIndexes  (getEmptyIndex board) [0..(length board)-1]
    in   [ActionDef "swap_with" [SelectIntDef "tile" options 1 1 False]]
  )
  else Left (InvalidParameter "invalid playerId")

applyAction :: PlayerId -> PlayerState -> ActionValue -> Either Error PlayerState
applyAction playerId (PlayerState players board target) action = undefined
