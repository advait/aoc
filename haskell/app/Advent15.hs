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
             Int
  deriving (Show, Eq, Ord)

-- Char to Piece
readPiece :: Char -> Maybe Piece
readPiece '#' = Just Wall
readPiece 'G' = Just $ Humanoid Goblin 200 3
readPiece 'E' = Just $ Humanoid Elf 200 3
readPiece '.' = Nothing
readPiece c   = error $ "Invalid piece: " ++ [c]

-- The state of the world where Pieces exist at Positions
type World = Map.Map Pos Piece

-- Parse puzzle input, returning a World
readWorld :: String -> World
readWorld s = Map.fromList pieces
  where
    zipWithIndex = zip [0 ..]
    mapLine (y, line) = map (\(x, c) -> (Pos x y, readPiece c)) (zipWithIndex line)
    maybePieces = concatMap mapLine . zipWithIndex $ lines s
    pieces = map (\(pos, p) -> (pos, Maybe.fromJust p)) $ filter (\(_, p) -> Maybe.isJust p) maybePieces

-- Prints the world in a format similar to what we see on Advent of Code for debugging.
showWorld :: World -> String
showWorld world = rec 0
  where
    rec :: Int -> String
    maxX = maximum . map (\(Pos x _) -> x) . Map.keys $ world
    maxY = maximum . map (\(Pos _ y) -> y) . Map.keys $ world
    poss = sort [Pos x y | x <- [0 .. maxX], y <- [0 .. maxY]]
    rec row
      | null pws = "" -- No more things to print
      | otherwise = rowString ++ rec (row + 1)
      where
        pws = sort . map (, world) . filter (\(Pos _ y) -> y == row) $ poss
        dungeonMap = map (showPieceShort . getPiece) pws
        pieceLegend =
          intercalate ", " . map showPieceLegend . Maybe.mapMaybe getPiece . filter (Maybe.isJust . getRace) $ pws
        rowString = dungeonMap ++ "  " ++ pieceLegend ++ "\n"
    showPieceShort :: Maybe Piece -> Char
    showPieceShort (Just Wall)                  = '#'
    showPieceShort (Just (Humanoid Goblin _ _)) = 'G'
    showPieceShort (Just (Humanoid Elf _ _))    = 'E'
    showPieceShort _                            = '.'
    showPieceLegend :: Piece -> String
    showPieceLegend (Humanoid Goblin h _) = "G(" ++ show h ++ ")"
    showPieceLegend (Humanoid Elf h _) = "E(" ++ show h ++ ")"
    showPieceLegend _ = error "Cannot show legend for Wall"

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
    Just (Humanoid r _ _) -> Just r
    _                     -> Nothing

-- Returns the attack power of an elf or a goblin
getAttackPower :: PosWorld -> Int
getAttackPower pw =
  case getPiece pw of
    Just (Humanoid _ _ p) -> p
    _                     -> error "Wall does not have attack power"

-- Returns the health of an elf or a goblin or 0 if the PosWorld is empty or contains a Wall
getHealth :: PosWorld -> Int
getHealth pw =
  case getPiece pw of
    Just (Humanoid _ h _) -> h
    _                     -> 0

-- Updates the health of the given piece
updateHealth :: Int -> PosWorld -> Piece
updateHealth health pw =
  case getPiece pw of
    Just (Humanoid race _ attackPower) -> Humanoid race health attackPower
    _ -> error "Cannot set health for non-player piece"

-- Returns all adjacent PosWorlds regardless of whether they are occupied
allNeighbors :: PosWorld -> [PosWorld]
allNeighbors (Pos x y, world) = map (, world) posNeighbors
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
    newHealth = getHealth weakestEnemy - getAttackPower pw
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

-- Returns whether the game is over (only one race remining)
isGameOver :: World -> Bool
isGameOver w = (== 1) . Set.size . Set.fromList . Maybe.mapMaybe (getRace . (, w)) $ Map.keys w

-- Represents the state of the game after a round
data RoundStatus
  = GameContinues -- The game is not over, battle continues
  | GameOverIncompleteRound -- The game completed in the middle of this round
  | GameOverCompleteRound -- The game completed as a consequence of the final player's turn signifying a full round

-- Steps through pieces in reading order, performs moves/attacks, and returns the final world as well as a RoundStatus.
playRound :: World -> (World, RoundStatus)
playRound world = rec world piecePositions
  where
    piecePositions = sort . filter (Maybe.isJust . getRace . (, world)) $ Map.keys world
    rec :: World -> [Pos] -> (World, RoundStatus)
    rec world []
      | isGameOver world = (world, GameOverCompleteRound)
      | otherwise = (world, GameContinues)
    rec world (pos:rest)
      | isGameOver world = (world, GameOverIncompleteRound)
      | otherwise = rec (play world pos) rest

-- Steps through all rounds until all Elves are dead, returning the number of rounds played.
playAllRounds :: Int -> World -> (Int, World)
playAllRounds round world
  | Trace.trace ("Round: " ++ show round) False = undefined
  | Trace.trace ("World: \n" ++ showWorld world) False = undefined
  | otherwise =
    case playRound world of
      (newWorld, GameContinues) -> playAllRounds (round + 1) newWorld
      (newWorld, GameOverIncompleteRound) -> (round, newWorld)
      (newWorld, GameOverCompleteRound) -> (round + 1, newWorld)

-- Play all rounds and return the summarized combat (sum of health times number of full rounds played)
summarizeCombat :: World -> Int
summarizeCombat startWorld = rounds * totalHealth
  where
    (rounds, finalWorld) = playAllRounds 0 startWorld
    totalHealth = sum . map (getHealth . (, finalWorld)) . Map.keys $ finalWorld

-- Updates the elf pieces, giving them the provided attack power.
updateElfAttackPower :: Int -> World -> World
updateElfAttackPower power world = newWorld
  where
    updateOne :: Piece -> Piece
    updateOne (Humanoid Elf health _) = Humanoid Elf health power
    updateOne other                   = other
    newWorld = Map.map updateOne world

-- Increments the elf attack power until an entire round happens where they don't die.
findMinimumPower :: World -> Int
findMinimumPower startWorld = rec 4 startWorld
  where
    getElfCount world = length . filter ((== Just Elf) . getRace) . map (, world) . Map.keys $ world
    startingElfCount = getElfCount startWorld
    rec :: Int -> World -> Int
    rec power world
      | Trace.trace ("Trying power: " ++ show power) False = undefined
      | Trace.trace ("Elf counts: " ++ show startingElfCount ++ " " ++ (show . getElfCount $ finalWorld)) False =
        undefined
      | getElfCount finalWorld == startingElfCount = rounds * totalHealth
      | otherwise = rec (power + 1) world
      where
        (rounds, finalWorld) = playAllRounds 0 (updateElfAttackPower power world)
        totalHealth = sum . map (getHealth . (, finalWorld)) . Map.keys $ finalWorld

main :: IO ()
main = do
  world <- readWorld <$> getContents
  let problem1 = summarizeCombat world
  let problem2 = findMinimumPower world
  --putStrLn $ "Problem 1: " ++ show problem1
  putStrLn $ "Problem 2: " ++ show problem2
