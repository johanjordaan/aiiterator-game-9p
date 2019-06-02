module Game where
import Shuffle
import Coord

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

type PlayerId = String
type Moves = Int
data Player = Player PlayerId Moves deriving (Show, Eq)

type Players = [Player]
type GameState = CoordSpace
data PlayerState = PlayerState {
  getDims::Dim
  getPlayers::Players,
  getTargetGameState::GameState,
  getCurrentGameState::GameState
} deriving Show


_initGame :: Dim -> IO PlayerState
_initGame d = do {
  shuffledBoard <- shuffle (consCoordSpace d);
  return $ PlayerState d [] (consCoordSpace d) shuffledBoard;
}

initGame :: Dim -> Either Error (IO PlayerState)
initGame d = if(validateDim d) then Right $ _initGame d else Left $ (InvalidParameter "invalid dim")

joinGame :: PlayerId -> PlayerState -> Either Error PlayerState
joinGame playerId (PlayerState d players current target) =
  if(length players ==0) then Right $ PlayerState d ((Player playerId 0):players) current target
  else Left $ (InvalidParameter "already at max (1) players")

getActions :: PlayerId -> PlayerState -> Either Error ActionDefs
getActions playerId (PlayerState d players current target) = let
  emptySpace = [1,1,1]
  possibleMoves = foldr (\i a-> (addToCoordInDim emptySpace i 1):(addToCoordInDim emptySpace i (-1)):a ) [] [0..(length d)-1]
  validMoves = filter (validateCoord d) (foldr (\i a-> (i+1):(i-1):a) [] d)
  in undefined





--validIndexes :: Int -> [Int] -> [Int]
--validIndexes i b =
--  let l = round $ sqrt $ fromIntegral (length b)
--  in filter (\t -> t>0 && t<(l*l)) [i-1,i+1,i-l,i+l]

--selectIndexes :: [Int] -> [a] -> [a]
--selectIndexes is l = map snd (filter (\i -> elem (fst i) is) (zip [0..] l))

--_getEmptyIndex :: Int -> Board -> Int
--_getEmptyIndex a [] = a
--_getEmptyIndex a (x:xs) = case x of Empty -> a; NotEmpty _ -> _getEmptyIndex (a+1) xs

--getEmptyIndex :: Board -> Int
--getEmptyIndex board = _getEmptyIndex 0 board

--selectIndexes (validIndexes  (getEmptyIndex sb) [0..9-1]) b

--getActions :: PlayerId -> PlayerState -> Either Error ActionDefs
--getActions playerId (PlayerState players board target) =
--  if (elem playerId (map (\(Player id _)->id) players)) then Right (
--    let  options = validIndexes  (getEmptyIndex board) [0..(length board)-1]
--    in   [ActionDef "swap_with" [SelectIntDef "tile" options 1 1 False]]
--  )
--  else Left (InvalidParameter "invalid playerId")

--applyAction :: PlayerId -> PlayerState -> ActionValue -> Either Error PlayerState
--applyAction playerId (PlayerState players board target) action = undefined
