module Advent13 where

import qualified Data.List  as List
import qualified Data.Map   as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set   as Set

-- (X, Y) Coordinates
type Pos = (Int, Int)

-- Cardinal directions
data Dir
  = DUp
  | DDown
  | DLeft
  | DRight
  deriving (Eq, Show)

-- Represents a mine cart
data Car =
  Car Pos
      Dir
      Int -- Number of intersections we have gone through
  deriving (Eq, Show)

instance Ord Car where
  compare (Car p1 _ _) (Car p2 _ _) = compare p1 p2

readCar :: Pos -> Char -> Maybe Car
readCar p '^' = Just $ Car p DUp 0
readCar p 'v' = Just $ Car p DDown 0
readCar p '<' = Just $ Car p DLeft 0
readCar p '>' = Just $ Car p DRight 0
readCar _ _   = Nothing

data Track
  = Vertical
  | Horizontal
  | Forwardslash
  | Backslash
  | Intersection
  | Empty

type TrackMap = Map.Map Pos Track

readTrack :: Char -> Track
readTrack '|'  = Vertical
readTrack '-'  = Horizontal
readTrack '/'  = Forwardslash
readTrack '\\' = Backslash
readTrack '^'  = Vertical
readTrack 'v'  = Vertical
readTrack '<'  = Horizontal
readTrack '>'  = Horizontal
readTrack '+'  = Intersection
readTrack _    = Empty

turnLeft :: Dir -> Dir
turnLeft DUp    = DLeft
turnLeft DDown  = DRight
turnLeft DLeft  = DDown
turnLeft DRight = DUp

turnRight :: Dir -> Dir
turnRight DUp    = DRight
turnRight DDown  = DLeft
turnRight DLeft  = DUp
turnRight DRight = DDown

nextDir :: Track -> Car -> Dir
nextDir Vertical (Car _ d _) = d
nextDir Horizontal (Car _ d _) = d
nextDir Forwardslash (Car _ DUp _) = DRight
nextDir Forwardslash (Car _ DDown _) = DLeft
nextDir Forwardslash (Car _ DLeft _) = DDown
nextDir Forwardslash (Car _ DRight _) = DUp
nextDir Backslash (Car _ DUp _) = DLeft
nextDir Backslash (Car _ DDown _) = DRight
nextDir Backslash (Car _ DLeft _) = DUp
nextDir Backslash (Car _ DRight _) = DDown
nextDir Intersection (Car _ d n)
  | nMod == 0 = turnLeft d
  | nMod == 1 = d
  | nMod == 2 = turnRight d
  | otherwise = error "Invalid"
  where
    nMod = n `mod` 3
nextDir Empty (Car pos dir _) = error $ "Car off track at: " ++ show pos ++ " going " ++ show dir

-- Return a pos that is a step in the given direction
stepDir :: Dir -> Pos -> Pos
stepDir DUp (x, y)    = (x, y - 1)
stepDir DDown (x, y)  = (x, y + 1)
stepDir DLeft (x, y)  = (x - 1, y)
stepDir DRight (x, y) = (x + 1, y)

-- Return a new car object after the given track has been applied
nextCar :: Car -> Track -> Car
nextCar car@(Car pos d n) track =
  case track of
    Intersection -> Car nextPos nd (n + 1)
    _            -> Car nextPos nd n
  where
    nd = nextDir track car
    nextPos = stepDir nd pos

-- Convenience to iterate over characters with Pos indexes
mapWithPos :: (Pos -> Char -> a) -> String -> [a]
mapWithPos fn input = concat foo -- flatten
  where
    ls = lines input
    mapSingleLine (y, line) = map (\(x, c) -> fn (x, y) c) (zip [0 ..] line)
    foo = zipWith (curry mapSingleLine) [0 ..] ls

-- Return the cars from the puzzle input
genCars :: String -> [Car]
genCars s = map Maybe.fromJust $ filter Maybe.isJust $ mapWithPos readCar s

-- Return the tracks from the puzzle input
genTracks :: String -> TrackMap
genTracks s = Map.fromList $ mapWithPos (\pos c -> (pos, readTrack c)) s

-- Returns whether there is a collision amongst the cars
isCollis :: [Car] -> Maybe Pos
isCollis = tempFn Set.empty
  where
    tempFn :: Set.Set Pos -> [Car] -> Maybe Pos
    tempFn _ [] = Nothing
    tempFn seenPoss (Car pos _ _:tail)
      | Set.member pos seenPoss = Just pos
      | otherwise = tempFn (Set.insert pos seenPoss) tail

-- Continually step until we have a collision, returning the position
stepUntilCollision :: [Car] -> [Car] -> TrackMap -> Pos
stepUntilCollision acc cars _
  | Maybe.isJust collis = Maybe.fromJust collis
  where
    allCars = acc ++ cars
    collis = isCollis allCars
stepUntilCollision acc [] tracks = stepUntilCollision [] (List.sort acc) tracks
stepUntilCollision acc (car@(Car pos _ _):tail) tracks = stepUntilCollision (nc : acc) tail tracks
  where
    track = tracks Map.! pos
    nc = nextCar car track

doMain :: String -> String
doMain stdin = show collisPos ++ "\n"
  where
    tracks = genTracks stdin
    cars = genCars stdin
    collisPos = stepUntilCollision [] cars tracks

main :: IO ()
main = interact doMain
