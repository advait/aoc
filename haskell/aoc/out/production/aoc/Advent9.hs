module Advent9 where

import qualified Data.IntMap as IntMap
import           Data.Maybe
import           Deque

numElves = 411
maxMarble = 72059 * 100
--numElves = 9
--maxMarble = 25

type Elf = Int

type Marble = Int

type Circle = Deque Marble

type ElfScores = IntMap.IntMap Int

data Game =
  Game Int -- Next Marble
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
playAllTurns :: Game -> Game
playAllTurns game@(Game marble _ _)
  | marble > maxMarble = game
  | otherwise = playAllTurns $! playSingleTurn game

-- Inserts a marble into the circle, returning a new circle
playNormalTurn :: Game -> Game
playNormalTurn (Game marble circle elfScores) = newGame
  where
    newCircle = Deque.cons marble $ applyN 2 Deque.shiftLeft circle
    newGame = Game (marble + 1) newCircle elfScores

--
play23Turn :: Game -> Game
play23Turn (Game marble circle elfScores) = newGame
  where
    (removedMarble, newCircle) = Data.Maybe.fromJust . Deque.uncons $ applyN 7 Deque.shiftRight circle
    elfID = marble `mod` numElves
    elfAddedPoints = marble + removedMarble
    newElfScores = IntMap.insertWith (+) elfID elfAddedPoints elfScores
    newGame = Game (marble + 1) newCircle newElfScores

-- Starting state for the game
startingGame = Game 1 (Deque.fromList [0]) IntMap.empty

main :: IO ()
main = print maxScore
  where
    (Game _ _ elfScores) = playAllTurns startingGame
    maxScore = IntMap.foldl max 0 elfScores
