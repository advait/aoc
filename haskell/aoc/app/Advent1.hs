module Main where

import qualified Data.IntSet as IntSet
import           Prelude

main :: IO ()
main =
  interact $ \s ->
    let input = parseInput s
        part1 = sum input
        part2 = startProcess input
     in "Part1: " ++ show part1 ++ "\n" ++ "Part2: " ++ show part2 ++ "\n"

-- Parse the input strings with preceding + or - values
intOfString :: String -> Int
intOfString "" = 0
intOfString s@(head:tail)
  | head == '+' = read tail :: Int -- Need to manually parse '+'
  | otherwise = read s :: Int

-- Processes stdin as a list of ints
parseInput :: String -> [Int]
parseInput s = map intOfString stringLines
  where
    stringLines = lines s

-- Processes the cycle returning the first duplicate sum
process :: [Int] -> Int -> IntSet.IntSet -> Int
process (head:tail) curSum seen =
  if IntSet.member curSum seen
    then curSum
    else process tail newSum newSeen
  where
    newSeen = IntSet.insert curSum seen
    newSum = curSum + head

-- Kickoff process with proper initial values
startProcess :: [Int] -> Int
startProcess numbers = process (cycle numbers) 0 IntSet.empty
