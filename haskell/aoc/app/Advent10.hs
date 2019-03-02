module Advent10 where

import qualified Data.List   as List
import qualified Data.Map    as Map
import qualified Data.Maybe  as Maybe
import qualified Data.String as String

-- Given five-element String, maybe return a plant substitution
type Matcher = String -> Maybe Char

-- String of plants with the Int representing the offset to the zero point
type Plants = (Int, String)

-- Int representing which Generation we are on
type GenID = Int

-- Map from padded plants to (GenID, zero-point)
type SeenStates = Map.Map String (GenID, Int)

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
    templates = drop 1 allLines -- Drop the first line as it is explanatory

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
mapPlants _ _ _ = error "Input was too short"

-- Returns the last n items from a List
lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

-- Pads the plants to make sure there's enough empty pots before the next round.
-- Potentially updates the zero index point if we have to pad left.
pad :: Int -> Plants -> Plants
pad n (z, s) = unpadLeft . unpadRight $ (n + z, padding ++ s ++ padding)
  where
    padding = replicate n '.'
    unpadLeft (z, s)
      | not $ all (== '.') (take (n + 1) s) = (z, s)
    unpadLeft (z, h:tail) = unpadLeft (z - 1, tail)
    unpadLeft (_, _) = error "Invalid list"
    unpadRight (z, s)
      | not $ all (== '.') (lastN (n + 1) s) = (z, s)
    unpadRight (z, s) = unpadRight (z, init s)

-- Create the next generation of plants given the matcher
nextGen :: Int -> (String -> String) -> Plants -> Plants
nextGen n transformInput plants = (newZ, output)
  where
    (z, input) = pad n plants
    output = transformInput input
    newZ = z - (n `div` 2) -- Transforming the input drops n/2 plants from the front

-- Runs all generations returning the final set of plants
allGens :: Int -> (String -> String) -> GenID -> SeenStates -> Plants -> Plants
allGens _ _ 0 _ plants = plants
allGens n transformAllPlants genID seen plants@(z, input) =
  case alreadySeen of
    Just (seenGenID, seenZ) -> rec (genID - gensToSkip) Map.empty (newZ, input)
      -- We've seen this state already, meaning there is a cycle. Compute the
      -- differences between this state and the last seen state (dGenID, dZPerCycle)
      -- and then fast-forward assuming dZPerCycle will apply every dGenID.
      where dGenID = seenGenID - genID -- How many generations within a cycle
            dZPerCycle = seenZ - z -- How much z moves per cycle
            gensToSkip = genID `div` dGenID -- Repeatedly apply the cycle this many times
            dZ = dZPerCycle * gensToSkip -- z must be changed by this much if we fast-forward gensToSkip
            newZ = z - dZ
    Nothing -> rec (genID - 1) nextSeen nextPlants
  where
    alreadySeen = Map.lookup input seen
    rec = allGens n transformAllPlants
    nextSeen = Map.insert input (genID, z) seen
    nextPlants = nextGen n transformAllPlants plants

-- Sum the pot numbers with plants in them
potCount :: Plants -> Int
potCount (_, "") = 0
potCount (z, plant:tail)
  | plant == '.' = potCount (z - 1, tail)
  | plant == '#' = currentPotNumber + potCount (z - 1, tail)
  where
    currentPotNumber = negate z
potCount (_, _) = error "Invalid plant symbol"

startingInput = "##.###.......#..#.##..#####...#...#######....##.##.##.##..#.#.##########...##.##..##.##...####..####"

matcherWidth = 5

nGens = 50000000000

runMain :: String -> String
runMain stdin = output
  where
    matchers = parseInput stdin
    transformNPlants = applyMatchers matchers
    transformAllPlants = mapPlants matcherWidth transformNPlants
    startingPlants = (0, startingInput)
    finalPlants = allGens matcherWidth transformAllPlants nGens Map.empty startingPlants
    output = show (potCount finalPlants) ++ "\n"

main :: IO ()
main = interact runMain
