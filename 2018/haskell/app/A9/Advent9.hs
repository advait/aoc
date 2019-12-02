module A9.Advent9 where

import qualified Data.IntMap as IntMap
import           Data.Maybe
import           Deque

type Elf = Int

type Marble = Int

type Circle = Deque Marble

type ElfScores = IntMap.IntMap Int

data Game =
  Game Marble
       Circle
       ElfScores
  deriving (Show)

-- Apply the given function n times
applyN :: Int -> (a -> a) -> a -> a
applyN 0 f start = start
applyN n f start = applyN (n - 1) f (f start)

-- Play a single turn
playSingleTurn :: Game -> Game
playSingleTurn game@(Game nextMarble _ _)
  | nextMarble `mod` 23 == 0 = play23Turn game
  | otherwise = playNormalTurn game

-- Plays all turns to completion
playAllTurns :: Marble -> Game -> Game
playAllTurns maxMarble game@(Game marble _ _)
  | marble > maxMarble = game
  | otherwise = playAllTurns maxMarble $ playSingleTurn game

-- Inserts a marble into the circle, returning a new circle
playNormalTurn :: Game -> Game
playNormalTurn (Game marble circle elfScores) = newGame
  where
    shiftedCircle = applyN 2 Deque.shiftLeft circle
    newCircle = Deque.cons marble shiftedCircle
    newGame = Game (marble + 1) newCircle elfScores

-- Plays the strange mod 23 turn that allows an elf to score points
play23Turn :: Game -> Game
play23Turn (Game marble circle elfScores) = newGame
  where
    shiftedCircle = applyN 7 Deque.shiftRight circle
    (removedMarble, newCircle) = Data.Maybe.fromJust . Deque.uncons $ shiftedCircle
    elfID = marble `mod` numElves
    elfAddedPoints = marble + removedMarble
    newElfScores = IntMap.insertWith (+) elfID elfAddedPoints elfScores
    newGame = Game (marble + 1) newCircle newElfScores

-- Starting state for the game
startingGame = Game 1 (Deque.fromList [0]) IntMap.empty

numElves = 411

part1MaxMarble = 72059

part2MaxMarble = part1MaxMarble * 100

main :: IO ()
main = putStrLn $ "Part 1: " ++ show part1MaxScore ++ "\nPart 2: " ++ show part2MaxScore
  where
    (Game _ _ part1Scores) = playAllTurns part1MaxMarble startingGame
    (Game _ _ part2Scores) = playAllTurns part2MaxMarble startingGame
    part1MaxScore = IntMap.foldl max 0 part1Scores
    part2MaxScore = IntMap.foldl max 0 part2Scores
