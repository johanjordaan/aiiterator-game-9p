module Coord where

calcDm :: [Int] -> [Int]
calcDm d = let
  d1 = head d;
  d2 = head (drop 1 d);
  r = reverse (drop 2 d);
  in foldr (\i a-> (foldr (*) 1 (take (length a) d )):a ) [d2,d1] r

rdiv ::  Int -> Int -> Int
rdiv a b = floor ((fromIntegral a) / (fromIntegral b))

rmod :: Real a => a -> a -> Int
rmod a b = (floor (realToFrac a)) `mod` (floor (realToFrac b))

calcR t dm =
    foldr (\i a -> (
      rdiv (snd (head a)) i,
      rmod (snd (head a)) i
      ):a
    )
    [(rdiv t (head dm), rmod t (head dm))]
    (reverse (tail dm))

type Dim = [Int]
type Coord = [Int]
type CoordSpace = [Coord]

zeroCoord :: Dim -> Coord
zeroCoord d = map (*0) [0..(length d)-1]

dimSize :: Dim -> Int
dimSize d = (foldr (*) 1 d)

toCoord :: Dim -> Int -> Coord
toCoord d i = let
  dm = calcDm d
  r = calcR i dm
  in foldr (\i a-> (fst i):a) [snd (head r)] (reverse (tail r))

fromCoord :: Dim -> Coord -> Int
fromCoord d c = let
  dm = reverse $ 1:(drop 1 (reverse (calcDm d)))
  in foldr (\i a->((fst i)*(snd i))+a) 0 (zip c dm)

incCoord :: Dim -> Coord -> Coord
incCoord d c = fst $ foldr (\i a -> let
    carry = snd a
    newCoord = fst a
    newValue = (fst i)+carry
    limit = snd i
    in if newValue==limit
      then (0:newCoord,1)
      else (newValue:newCoord,0)
  )
  ([],1)
  (zip c d)

validateDim :: Dim -> Bool
validateDim d = foldr (\i a -> let
    p = i>0
    in p && a
  )
  True
  d

validateCoord :: Dim -> Coord -> Bool
validateCoord d c = foldr (\i a -> let
    value = fst i
    limit = snd i
    in value<limit && value>=0 && a
  )
  True
  (zip c d)

addToCoordInDim :: Coord -> Int -> Int -> Coord
addToCoordInDim c di v = let
  beginning = take di c
  middle = head (drop di c)
  end = drop (di+1) c
  in beginning++((middle+v):end)

consCoordSpace :: Dim -> CoordSpace
consCoordSpace d = reverse $ foldr (\i a -> let
    lastCoord = head a
    in (incCoord d lastCoord):a
  )
  [zeroCoord d]
  [1..(dimSize d)-1]
