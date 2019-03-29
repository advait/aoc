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
  deriving (Show, Eq, Ord)

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
-- TODO(advait): Consider using monadic approaches to introspecting and modifying PosWorld.
type PosWorld = (Pos, World)

-- Maybe returns the piece at the given PosWorld
getPiece :: PosWorld -> Maybe Piece
getPiece (pos, world) = Map.lookup pos world

-- Maybe returns the race of the humanoid at the given PosWorld
getRace :: PosWorld -> Maybe Race
getRace pw =
  case getPiece pw of
    Just (Humanoid r _) -> Just r
    _                   -> Nothing

-- Returns the health of an elf or a goblin
getHealth :: PosWorld -> Int
getHealth pw =
  case getPiece pw of
    Just (Humanoid _ h) -> h
    _                   -> 0

-- Updates the health of the given piece
updateHealth :: Int -> PosWorld -> Piece
updateHealth h pw =
  case getPiece pw of
    Just (Humanoid r _) -> Humanoid r h
    _                   -> error "Cannot set health for non-player piece"

-- Returns all adjacent PosWorlds regardless of whether they are occupied
allNeighbors :: PosWorld -> [PosWorld]
allNeighbors (Pos x y, world) = map (,world) posNeighbors
  where
    posNeighbors = sort [Pos (x - 1) y, Pos (x + 1) y, Pos x (y - 1), Pos x (y + 1)]

-- Represents a pathfinding view of the world from the perspective of the given race. A search from the perspective
-- of a goblin will be different than a search from the perspective of an elf (i.e. goblins will find other goblin
-- PosWorlds as non-empty while it will find elf tiles as traversable). As a result, we must note the Race of the
-- starting tile when we start our search.
type SearchNode = (PosWorld, Race)

instance Node SearchNode where
  neighbors (pw, startRace) = map (, startRace) . filter enemyOrEmpty . allNeighbors $ pw
    where
      enemyOrEmpty pw = isEnemy (Just startRace) (getRace pw) || Maybe.isNothing (getPiece pw)

-- Returns whether the two pieces are enemies
isEnemy :: Maybe Race -> Maybe Race -> Bool
isEnemy (Just Goblin) (Just Elf) = True
isEnemy (Just Elf) (Just Goblin) = True
isEnemy _ _                      = False

-- Returns adjacent neighbors that are enemies
enemyNeighbors :: PosWorld -> [PosWorld]
enemyNeighbors pw = filter (isEnemy (getRace pw) . getRace) $ allNeighbors pw

-- If possible, performs an attack turn, reducing the hitpoints of an enemy, removing it if it dies.
-- Returns the updated world. Performs a noop if no attack is possible.
attack :: PosWorld -> World
attack pw@(pos, world)
  | null . enemyNeighbors $ pw = world -- No enemies nearby, noop
  | newHealth <= 0 = Map.delete enemyPos world
  | otherwise = Map.insert enemyPos newPiece world
  where
    compareByHealth pw1 pw2 = compare (getHealth pw1) (getHealth pw2)
    weakestEnemy@(enemyPos, _) = minimumBy compareByHealth $ enemyNeighbors pw
    newHealth = getHealth weakestEnemy - attackPower
    newPiece = updateHealth newHealth weakestEnemy

-- If necessary and possible to move, performs a move turn, moving towards the nearest enemy
-- in reading order. Otherwise performs noop. Returns the updated PosWorld.
move :: PosWorld -> PosWorld
move pw@(pos, world)
  | not . null . enemyNeighbors $ pw = pw -- No moves necessary, we can attack
  | otherwise = newPosWorld
  where
    piece = Maybe.fromJust $ getPiece pw
    startNode = (pw, Maybe.fromJust . getRace $ pw)
    isFinishNode (pw, startRace) = isEnemy (Just startRace) $ getRace pw
    preferredNextPos = (!! 1) . map fst <$> shortestPathBool startNode isFinishNode
    newPosWorld =
      case preferredNextPos of
        Nothing -> pw
        Just (nextPos, world) -> (nextPos, newWorld)
          where newWorld = Map.insert nextPos piece . Map.delete pos $ world

-- Play a single turn for the piece at the given PosWorld, returning a new World as a result of the move.
play :: World -> Pos -> World
play world pos
  | Maybe.isNothing . getRace $ (pos, world) = world -- Inanimate, noop
  | otherwise = attack . move $ (pos, world)

-- Steps through all pieces in reading order, performs moves, and returns the state of the world after one full found.
playRound :: World -> World
playRound world = foldl play world piecePositions
  where
    piecePositions = sort . filter (Maybe.isJust . getRace . (, world)) $ Map.keys world

-- Steps through all rounds until all Elves are dead, returning the number of rounds played.
playAllRounds :: Int -> World -> (Int, World)
playAllRounds round world
  | onlyOneRace world = (round, world) -- We started with a finished world
  | onlyOneRace newWorld = (round, newWorld)
  | otherwise = playAllRounds (round + 1) newWorld
  where
    onlyOneRace w = (== 1) . Set.size . Set.fromList . Maybe.mapMaybe (getRace . (,w)) $ Map.keys w
    newWorld = playRound world

-- Play all rounds and return the summarized combat (sum of health times number of full rounds played)
summarizeCombat :: World -> Int
summarizeCombat startWorld = rounds * totalHealth
  where
    (rounds, finalWorld) = playAllRounds 0 startWorld
    totalHealth = sum . map (getHealth . (,finalWorld)) . Map.keys $ finalWorld

attackPower = 3

startingHealth = 200

main :: IO ()
main = interact (show . summarizeCombat . readWorld)
