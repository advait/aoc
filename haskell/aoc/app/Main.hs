module Main where

import qualified Data.IntSet as IntSet
import Debug.Trace
import Prelude

main :: IO ()
main = interact $ show . myMain

-- Parse the input strings with preceeding + or - values
intOfString :: String -> Int
intOfString "" = 0
intOfString s@(head:tail)
  | head == '+' = read tail::Int -- Need to manually parse '+'
  | otherwise = read s::Int

-- Processes the cycle returning the first duplicate sum
process :: Int -> [Int] -> IntSet.IntSet -> Int
process curSum (head:tail) seen =
  if IntSet.member curSum seen
  then curSum
  else process newSum tail newSeen where
    newSeen = IntSet.insert curSum seen
    newSum = curSum + head

myMain :: String -> Int
myMain s = process 0 infiniteCycle IntSet.empty where
  stringLines = lines s
  intList = map intOfString stringLines
  infiniteCycle = cycle intList
