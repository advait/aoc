module Advent14 where

import qualified Data.IntMap as IntMap
import qualified Data.Maybe  as Maybe
import qualified Debug.Trace as Trace

type Recipe = Int

type Recipes = IntMap.IntMap Recipe

type ElfPos = Int

data State =
  State Recipes -- Recipes we have accumulated
        ElfPos  -- Position of first elf
        ElfPos  -- Position of second elf
        Int     -- Length of Recipes

-- Explode an Int by its digits
explodeInt :: Int -> [Int]
explodeInt n = map charToInt (show n)
  where
    charToInt c = read [c] :: Int

-- Generate the next state from the current one
step :: State -> State
step (State rs e1 e2 nextIndex) = State newRs newE1 newE2 newNextIndex
  where
    r1 = rs IntMap.! e1
    r2 = rs IntMap.! e2
    newRecipes = explodeInt (r1 + r2)
    insertRecipe index recipe rs =
      let nextRs = IntMap.insert index recipe rs
       in (index + 1, nextRs)
    (newNextIndex, newRs) = foldl (\(index, rs) recipe -> insertRecipe index recipe rs) (nextIndex, rs) newRecipes
    newE1 = (e1 + r1 + 1) `mod` newNextIndex
    newE2 = (e2 + r2 + 1) `mod` newNextIndex

startingState = State (IntMap.fromList [(0, 3), (1, 7)]) 0 1 2

-- Generate at least n recipes, returning the final state
genNRecipes :: Int -> State -> State
genNRecipes n state@(State _ _ _ generatedRecipes)
  | n <= generatedRecipes = state
genNRecipes n state = genNRecipes n (step state)

-- Returns whether the given candidate search matches at the given position
candidateMatchesAt :: [Int] -> Int -> Recipes -> Bool
candidateMatchesAt [] _ _ = True
candidateMatchesAt (c:t) i rs
  | (rs IntMap.! i) /= c = False
candidateMatchesAt (_:t) i rs = candidateMatchesAt t (i + 1) rs

-- Returns the 10 recipe scores after we've generated n recipes
solvePart1 n = showRecipes endRecipes
  where
    recipesToGen = n + 10
    (State recipes _ _ _) = genNRecipes recipesToGen startingState
    endRecipes = map (recipes IntMap.!) [n .. n + 9]
    showRecipes = map (head . show) -- Convert [Recipe] to String

-- Generates recipes until we see the given recipes in a row, returning the index of the match
genRecipesUntil :: Int -> Int -> State -> Int
genRecipesUntil i = rec
  where
    toFind = explodeInt i
    toFindLen = length toFind -- Warning: This might not be 6 if we have preceding zeroes!
    rec :: Int -> State -> Int
    rec candidateIndex state@(State _ _ _ nextIndex) -- Not enough recipes generated to perform search
      | (candidateIndex + toFindLen) > nextIndex = rec candidateIndex $ step state
    rec candidateIndex state@(State rs _ _ nextIndex)
      | candidateMatchesAt toFind candidateIndex rs = candidateIndex -- Found match
      | otherwise = rec (candidateIndex + 1) state -- Did not find match

main :: IO ()
main = do
  let input = 825401
  let p1 = solvePart1 input
  let p2 = show $ genRecipesUntil input 0 startingState
  putStrLn $ "Part 1: " ++ p1 ++ "\nPart 2: " ++ p2
