module Advent15 where

import           Data.List
import qualified Data.Map    as Map
import qualified Data.Maybe  as Maybe
import qualified Data.Set    as Set
import qualified Debug.Trace as Trace
import           Pathfinding

-- (X, Y) cartesian coordinates
data Pos =
  Pos Int
      Int
  deriving (Eq)

-- Order positions according to "reading order": y first then x.
instance Ord Pos where
  compare (Pos x1 y1) (Pos x2 y2) = compare (y1, x1) (y2, x2)

-- Prints as (X, Y)
instance Show Pos where
  show (Pos x y) = show (x, y)

-- Represents a non-empty thing in the world
data Piece
  = Wall
  | Goblin Int
  | Elf Int
  deriving (Show, Eq)

startingHealth = 200

-- Char to Piece
readPiece :: Char -> Maybe Piece
readPiece '#' = Just Wall
readPiece 'G' = Just $ Goblin startingHealth
readPiece 'E' = Just $ Elf startingHealth
readPiece _   = Nothing

-- The state of the world where Pieces exist at Positions
type World = Map.Map Pos Piece

-- A specific position in the world, potentially containing a piece
data WorldPos =
  WorldPos World
           Pos
  deriving (Eq)

-- Maybe returns the piece at the given WorldPos
getPiece :: WorldPos -> Maybe Piece
getPiece (WorldPos world pos) = Map.lookup pos world

-- Returns whether there is a goblin at WorldPos
isGoblin :: WorldPos -> Bool
isGoblin wp =
  case getPiece wp of
    Just (Goblin _) -> True
    _               -> False

-- Returns whether there is an elf at WorldPos
isElf :: WorldPos -> Bool
isElf wp =
  case getPiece wp of
    Just (Elf _) -> True
    _            -> False

-- Returns the health of an elf or a goblin
getHealth :: WorldPos -> Int
getHealth wp =
  case getPiece wp of
    Just (Goblin h) -> h
    Just (Elf h) -> h
    _            -> 0

-- Updates the health of the given piece
updateHealth :: Int -> WorldPos -> Piece
updateHealth h wp =
  case getPiece wp of
    Just (Goblin _) -> Goblin h
    Just (Elf _) -> Elf h
    _ -> error "Cannot set health for non-player piece"

-- Returns whether the given WorldPos is dead
isDead :: WorldPos -> Bool
isDead = (<= 0) . getHealth

-- Compares two WorldPoss in order of health
compareByHealth :: WorldPos -> WorldPos -> Ordering
compareByHealth wp1 wp2 = compare (getHealth wp1) (getHealth wp2)

-- Prints the pos + piece at this WorldPos
instance Show WorldPos where
  show wp@(WorldPos world pos) = show pos ++ ": " ++ show (getPiece wp)

-- WorldPos are ordered based on the underlying pos
instance Ord WorldPos where
  compare (WorldPos _ pos1) (WorldPos _ pos2) = compare pos1 pos2

-- Returns all adjacent WorldPoss regardless of whether they are occupied
allNeighbors :: WorldPos -> [WorldPos]
allNeighbors (WorldPos world (Pos x y)) = map (WorldPos world) posNeighbors
  where
    posNeighbors = sort [Pos (x - 1) y, Pos (x + 1) y, Pos x (y - 1), Pos x (y + 1)]

-- Returns adjacent neighbors that don't have pieces
emptyNeighbors :: WorldPos -> [WorldPos]
emptyNeighbors = filter (Maybe.isNothing . getPiece) . allNeighbors

-- WorldPos is an instance of Node for pathfinding, but only finds paths through empty tiles.
instance Node WorldPos where
  neighbors = emptyNeighbors

-- Returns adjacent neighbors that are enemies
enemyNeighbors :: WorldPos -> [WorldPos]
enemyNeighbors wp
  | isGoblin wp = filter isElf $ allNeighbors wp
  | isElf wp = filter isGoblin $ allNeighbors wp
  | otherwise = []

-- Returns the weakest neighbor if it exists
weakestNeighbor :: WorldPos -> Maybe WorldPos
weakestNeighbor = Maybe.listToMaybe . enemyNeighbors

-- Alternative compare function for [Node]/Paths that prefers shorter paths
-- before going into a node-by-node comparison if paths are equal length.
comparePath :: (Node a) => [a] -> [a] -> Ordering
comparePath a b
  | lenA < lenB = LT
  | lenA > lenB = GT
  | otherwise = compare a b
  where
    lenA = length a
    lenB = length b

-- Performs an attack turn, reducing the hitpoints of the enemy, removing it if it dies.
attack :: WorldPos -> World
attack wp@(WorldPos world pos)
  | newHealth <= 0 = Map.delete pos world
  | otherwise = Map.insert pos newPiece world
  where
    enemy = Maybe.fromJust $ weakestNeighbor wp
    newHealth = getHealth enemy - 3
    newPiece = updateHealth newHealth enemy

-- Play a single turn for the piece at the given WorldPos, returning a new World
-- as a result of the move.
play :: WorldPos -> World
play wp = undefined

-- Given the state of the world, an accumulator for pieces that have already played,
-- and a list of pieces that have yet to play, make moves for all pieces, returning
-- the final world.
playRound :: World -> [Pos] -> [Pos] -> World
playRound w acc pieces = undefined

main :: IO ()
main = undefined
