module Day3 where

import Parser
import Prelude
import Control.Alternative (empty, (<|>))
import Data.Array (filter, index, length, snoc)
import Data.Foldable (product)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (logShow)

type Tree
  = Boolean

type TreePlane
  = Array (Array Tree)

treeParser :: Parser Tree
treeParser =
  empty
    <|> (const true <$> charP '#')
    <|> (const false <$> charP '.')

treePlaneParser :: Parser TreePlane
treePlaneParser =
  let
    rowParser = repeated1 treeParser
  in
    delimitedBy (charP '\n') rowParser

-- | Represents a zero-indexed position from the top-left corner
type Pos
  = { x :: Int, y :: Int }

type Slope
  = Pos -> Pos

makeSlope :: Int -> Int -> Pos -> Pos
makeSlope dx dy { x, y } = { x: x + dx, y: y + dy }

-- | Gets the tree at the given location, handling horizontal tiling.
getTree :: Pos -> TreePlane -> Maybe Tree
getTree { x, y } plane = do
  row <- index plane y
  let
    x' = x `mod` (length row)
  index row x'

countTreesInLine :: TreePlane -> Slope -> Int
countTreesInLine plane slope =
  let
    rec acc pos' = case getTree pos' plane of
      Nothing -> acc
      Just tree -> rec (snoc acc tree) (slope pos')
  in
    length $ filter identity $ rec [] { x: 0, y: 0 }

partA :: TreePlane -> Int
partA plane = countTreesInLine plane (makeSlope 3 1)

partB :: TreePlane -> Int
partB plane =
  let
    slopes =
      [ makeSlope 1 1
      , makeSlope 3 1
      , makeSlope 5 1
      , makeSlope 7 1
      , makeSlope 1 2
      ]
  in
    product $ countTreesInLine plane <$> slopes

main :: Effect Unit
main = do
  plane <- parsePuzzleInput "inputs/3.txt" treePlaneParser
  logShow $ partA plane
  logShow $ partB plane
