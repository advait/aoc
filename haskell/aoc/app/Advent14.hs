module Advent14 where

import qualified Data.IntMap as IntMap
import qualified Data.Maybe  as Maybe

type Recipe = Int

type Recipes = IntMap.IntMap Recipe

type ElfPos = Int

data State =
  State Recipes
        ElfPos
        ElfPos
        Int

-- Explode an Int by its digits
explodeInt :: Int -> [Int]
explodeInt n = map charToInt (show n)
  where
    charToInt c = read [c] :: Int

-- Generate the next state from the current one
step :: State -> State
step (State rs e1 e2 nextIndex)
  | rs `seq` e1 `seq` e2 `seq` False = undefined
step (State rs e1 e2 nextIndex) = State newRs newE1 newE2 newNextIndex
  where
    r1 = Maybe.fromJust $ IntMap.lookup e1 rs
    r2 = Maybe.fromJust $ IntMap.lookup e2 rs
    newRecipes = explodeInt (r1 + r2)
    insertRecipe index recipe rs =
      let nextRs = IntMap.insert index recipe rs
       in (index + 1, nextRs)
    (newNextIndex, newRs) = foldl (\(index, rs) recipe -> insertRecipe index recipe rs) (nextIndex, rs) newRecipes
    newE1 = (e1 + r1 + 1) `mod` newNextIndex
    newE2 = (e2 + r2 + 1) `mod` newNextIndex

startingState = State (IntMap.fromList [(0, 3), (1, 7)]) 0 1 2

genNRecipes :: Int -> State -> State
genNRecipes n state@(State _ _ _ generatedRecipes) | n <= generatedRecipes = state
genNRecipes n state = genNRecipes n (step state)

-- Returns the 10 recipe scores after we've generated n recipes
getRecipeScores n = showRecipes endRecipes
  where
    recipesToGen = n + 10
    (State recipes _ _ _) = genNRecipes recipesToGen startingState
    endRecipes = map (\i -> Maybe.fromJust $ IntMap.lookup i recipes) [n..n+9]

showRecipes :: [Recipe] -> String
showRecipes = map (stringToChar . show)
  where
    stringToChar [s] = s
    stringToChar _ = error "Too many chars in string"

main :: IO ()
main = putStrLn (getRecipeScores 825401)
