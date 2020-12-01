module Day1 where

import Parser
import Prelude
import Data.Array (catMaybes, elemIndex, head)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log, logShow, error)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Undefined (undefined)

inputParser :: Parser (Array Int)
inputParser = delimitedBy (stringP "\n") intParser

parseInput :: forall a. String -> Parser a -> Effect a
parseInput puzzle parser = do
  fileContents <- readTextFile UTF8 puzzle
  case runParserEof parser fileContents of
    Nothing -> undefined
    Just p -> pure p

partA :: Int -> Array Int -> Maybe Int
partA target input =
  let
    subs :: Array Int
    subs = ((-) target) <$> input

    contains :: forall a. Eq a => a -> Array a -> Maybe a
    contains a haystack = const a <$> elemIndex a haystack

    needle = head $ catMaybes $ (\sub -> contains sub input) <$> subs
  in
    (\x -> x * (target - x)) <$> needle

partB :: Array Int -> Maybe Int
partB input =
  let
    targets = ((-) 2020) <$> input

    checkTarget target = ((*) target) <$> partA target input
  in
    head $ catMaybes $ checkTarget <$> targets

main :: Effect Unit
main = do
  expenses <- parseInput "inputs/1.txt" inputParser
  logShow $ partA 2020 expenses
  logShow $ partB expenses
