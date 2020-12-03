module Test.Day1 where

import Day1
import Prelude
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Day 1" do
    let
      input = [ 1721, 979, 366, 299, 675, 1456 ]
    it "solves part A" do
      (partA 2020 input) `shouldEqual` (Just [ 299, 1721 ])
    it "solves part B" do
      (partB input) `shouldEqual` (Just [ 979, 675, 366 ])
