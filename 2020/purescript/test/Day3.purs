module Test.Day3 where

import Day3
import Parser
import Prelude
import Data.Array (replicate)
import Data.Maybe (Maybe(..), isJust)
import Data.String (joinWith, trim)
import Test.QuickCheck ((===))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.QuickCheck (quickCheck)

spec :: Spec Unit
spec = do
  describe "Day 3" do
    let
      parse s = (\p -> p.p) <$> runParser treePlaneParser s
    describe "parser" do
      let
        inputRow = "..##......."

        targetRow = [ false, false, true, true, false, false, false, false, false, false, false ]
      it "parses row properly" do
        parse inputRow `shouldEqual` Just [ targetRow ]
      it "parses multiple rows properly" do
        quickCheck \n ->
          let
            n' = (n `mod` 100) + 1
          in
            (parse $ joinWith "\n" $ replicate n' inputRow) === (Just $ replicate n' targetRow)
    describe "part A" do
      it "should solve part A for the exmaple"
        $ (partA <$> parse p1) `shouldEqual` (Just 7)

p1 :: String
p1 =
  trim
    """
..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#
"""
