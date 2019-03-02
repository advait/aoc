module Advent10 where

import qualified Data.List   as List
import qualified Data.Maybe  as Maybe
import qualified Data.String as String

-- Given five-element String, maybe return a plant substitution
type Matcher = String -> Maybe Char

-- String of plants with the Int representing the offset to the zero point
type Plants = (Int, String)

-- Given an input string, generate a matcher
genMatcher :: String -> Matcher
genMatcher template input =
  let firstSpaceIndex = Maybe.fromJust $ List.elemIndex ' ' template
      prefix = take firstSpaceIndex template
      output = last template
   in if prefix == input
        then Just output
        else Nothing

-- Given a puzzle input, return a list of matchers
parseInput :: String -> [Matcher]
parseInput s = map genMatcher templates
  where
    allLines = filter (/= "") $ lines s
    templates = drop 1 allLines -- Drop the two first lines as they are explanatory

-- Apply a List of matchers, returning the substitution
applyMatchers :: [Matcher] -> String -> Char
applyMatchers [] s = error $ "Unmatched String: " ++ s
applyMatchers (m:tail) s =
  case m s of
    Just c -> c
    _      -> applyMatchers tail s

-- Iterate over plants in n-unit chunks, transforming hunks with matchers
mapPlants :: Int -> (String -> Char) -> String -> String
mapPlants n _ input
  | length input < n = input
mapPlants n fn input@(_:tail) =
  let matchableInput = take n input
      transformedPlant = fn matchableInput
   in transformedPlant : mapPlants n fn tail

-- Returns the last n items from a List
lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

-- Pads the plants to make sure there's enough empty pots before the next round.
-- Potentially updates the zero index point if we have to pad left.
pad :: Int -> Plants -> Plants
pad n = padLeft . padRight
  where
    padLeft (z, s)
      | all (== '.') (take n s) = (z, s)
    padLeft (z, s) = padLeft (z + 1, '.' : s)
    padRight (z, s)
      | all (== '.') (lastN n s) = (z, s)
    padRight (z, s) = padRight (z, s ++ ".")

-- Create the next generation of plants given the matcher
nextGen :: Int -> (String -> String) -> Plants -> Plants
nextGen n transformInput plants = (newZ, output)
  where
    (z, input) = pad n plants
    output = transformInput input
    newZ = z - (n `div` 2) -- Transforming the input drops n/2 plants from the front

-- Runs all generations returning the final set of plants
allGens :: Int -> Int -> (String -> String) -> Plants -> Plants
allGens 0 _ _ plants = plants
allGens gens n transformAllPlants plants = allGens (gens - 1) n transformAllPlants nextPlants
  where
    nextPlants = nextGen n transformAllPlants plants

-- Sum the pot numbers with plants in them
potCount :: Plants -> Int
potCount (_, "") = 0
potCount (z, plant:tail)
  | plant == '.' = potCount (z - 1, tail)
potCount (z, plant:tail)
  | plant == '#' = currentPotNumber + potCount (z - 1, tail)
  where
    currentPotNumber = negate z

startingInput = "##.###.......#..#.##..#####...#...#######....##.##.##.##..#.#.##########...##.##..##.##...####..####"

matcherWidth = 5

nGens = 20

runMain :: String -> String
runMain stdin = output
  where
    matchers = parseInput stdin
    transformNPlants = applyMatchers matchers
    transformAllPlants = mapPlants matcherWidth transformNPlants
    startingPlants = (0, startingInput)
    finalPlants = allGens nGens matcherWidth transformAllPlants startingPlants
    output = show $ potCount finalPlants

main :: IO ()
main = interact runMain
