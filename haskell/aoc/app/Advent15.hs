{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections     #-}

module Advent15 where

import           Data.List
import qualified Data.Map    as Map
import qualified Data.Maybe  as Maybe
import qualified Data.Set    as Set
import qualified Debug.Trace as Trace
import           Pathfinding
import qualified System.IO   as IO

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

data Race
  = Goblin
  | Elf
  deriving (Show, Eq, Ord)

-- Represents a non-empty thing in the world
data Piece
  = Wall
  | Humanoid Race
             Int
  deriving (Show, Eq)

-- Char to Piece
readPiece :: Int -> Char -> Maybe Piece
readPiece _ '#'      = Just Wall
readPiece health 'G' = Just $ Humanoid Goblin health
readPiece health 'E' = Just $ Humanoid Elf health
readPiece _ '.'      = Nothing
readPiece _ c        = error $ "Invalid piece: " ++ [c]

-- The state of the world where Pieces exist at Positions
type World = Map.Map Pos Piece

-- Parse puzzle input, returning a World
readWorld :: String -> World
readWorld s = Map.fromList pieces
  where
    zipWithIndex = zip [0 ..]
    mapLine (y, line) = map (\(x, c) -> (Pos x y, readPiece startingHealth c)) (zipWithIndex line)
    maybePieces = concatMap mapLine . zipWithIndex $ lines s
    pieces = map (\(pos, p) -> (pos, Maybe.fromJust p)) $ filter (\(_, p) -> Maybe.isJust p) maybePieces

-- A specific position in the world, potentially containing a piece
-- TODO(advait): Consider whether this should be a monad?
data WorldPos =
  WorldPos World
           Pos
  deriving (Eq)

-- Maybe returns the piece at the given WorldPos
getPiece :: WorldPos -> Maybe Piece
getPiece (WorldPos world pos) = Map.lookup pos world

-- Returns the race of the humanoid
getRace :: WorldPos -> Maybe Race
getRace wp =
  case getPiece wp of
    Just (Humanoid r _) -> Just r
    _                   -> Nothing

-- Returns whether there is a goblin at WorldPos
isGoblin :: WorldPos -> Bool
isGoblin wp = getRace wp == Just Goblin

-- Returns whether there is an elf at WorldPos
isElf :: WorldPos -> Bool
isElf wp = getRace wp == Just Elf

-- Returns the health of an elf or a goblin
getHealth :: WorldPos -> Int
getHealth wp =
  case getPiece wp of
    Just (Humanoid _ h) -> h
    _                   -> 0

-- Updates the health of the given piece
updateHealth :: Int -> WorldPos -> Piece
updateHealth h wp =
  case getPiece wp of
    Just (Humanoid r _) -> Humanoid r h
    _                   -> error "Cannot set health for non-player piece"

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

-- Represents a view of the world from the perspective of the given race.
-- WorldPosContext is an instance of Node for pathfinding, but only finds paths through empty tiles or enemies.
type WorldPosContext = (WorldPos, Race)

instance Node WorldPosContext where
  neighbors (wp, startRace) = map (, startRace) . filter enemyOrEmpty . allNeighbors $ wp
    where
      enemyOrEmpty wp = isEnemy (Just startRace) (getRace wp) || Maybe.isNothing (getPiece wp)

-- Returns whether the two pieces are enemies
isEnemy :: Maybe Race -> Maybe Race -> Bool
isEnemy (Just Goblin) (Just Elf) = True
isEnemy (Just Elf) (Just Goblin) = True
isEnemy _ _                      = False

-- Returns adjacent neighbors that are enemies
enemyNeighbors :: WorldPos -> [WorldPos]
enemyNeighbors wp = filter (isEnemy (getRace wp) . getRace) $ allNeighbors wp

-- If possible, performs an attack turn, reducing the hitpoints of an enemy, removing it if it dies.
-- Returns the updated world. Performs a noop if no attack is possible.
attack :: WorldPos -> World
attack wp@(WorldPos world pos)
  | null . enemyNeighbors $ wp = world -- No enemies nearby, noop
  | newHealth <= 0 = Map.delete enemyPos world
  | otherwise = Map.insert enemyPos newPiece world
  where
    compareByHealth wp1 wp2 = compare (getHealth wp1) (getHealth wp2)
    weakestEnemy@(WorldPos _ enemyPos) = minimumBy compareByHealth $ enemyNeighbors wp
    newHealth = getHealth weakestEnemy - attackPower
    newPiece = updateHealth newHealth weakestEnemy

-- If necessary and possible to move, performs a move turn, moving towards the nearest enemy
-- in reading order. Otherwise performs noop. Returns the updated WorldPos.
move :: WorldPos -> WorldPos
move wp@(WorldPos world pos)
  | not . null . enemyNeighbors $ wp = wp -- No moves necessary, we can attack
  | otherwise = newWorldPos
  where
    piece = Maybe.fromJust $ getPiece wp
    goal (wp, startRace) = isEnemy (Just startRace) $ getRace wp
    startNode = (wp, Maybe.fromJust . getRace $ wp)
    preferredPath = map fst <$> shortestPathBool startNode goal
    preferredNextPos = (!! 1) <$> preferredPath
    newWorldPos =
      case preferredNextPos of
        Nothing -> wp
        Just (WorldPos _ nextPos) -> WorldPos newWorld nextPos
          where newWorld = Map.insert nextPos piece . Map.delete pos $ world

-- Play a single turn for the piece at the given WorldPos, returning a new World
-- as a result of the move.
play :: World -> Pos -> World
play world pos
  | not (isGoblin wp || isElf wp) = world -- Inanimate, noop
  | otherwise = attack . move $ wp
  where
    wp = WorldPos world pos
    shouldAttack = not . null $ enemyNeighbors wp

-- Steps through all pieces in reading order, performs moves, and returns the state
-- of the world after one full found.
playRound :: World -> World
playRound world = foldl play world allPos
  where
    allPos = sort $ Map.keys world

-- Steps through all rounds until all Elves are dead, returning the number of rounds played.
playAllRounds :: Int -> World -> (Int, World)
playAllRounds round world
  | isGameOver world = (round, world) -- We started with a finished world
  | isGameOver newWorld = (round, newWorld)
  | otherwise = playAllRounds (round + 1) newWorld
  where
    allElvesDead w = all (not . isElf) . map (WorldPos w) . Map.keys $ w
    allGoblinsDead w = all (not . isGoblin) . map (WorldPos w) . Map.keys $ w
    isGameOver w = allElvesDead w || allGoblinsDead w
    newWorld = playRound world

-- Play all rounds and return the summarized combat (sum of health times number of full rounds played)
summarizeCombat :: World -> Int
summarizeCombat startWorld = rounds * totalHealth
  where
    (rounds, finalWorld) = playAllRounds 0 startWorld
    totalHealth = sum . map (getHealth . WorldPos finalWorld) . Map.keys $ finalWorld

attackPower = 3

startingHealth = 200

main :: IO ()
main = interact (show . summarizeCombat . readWorld)
