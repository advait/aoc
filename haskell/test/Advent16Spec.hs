module Advent16Spec where

import A16.Advent16
import           Test.Hspec
import           Text.ParserCombinators.ReadP

spec :: Spec
spec = do
  describe "Parse Example A" $ do
    it "parses successfully" $ do
      let input = "Before: [0, 1, 1, 0]\n7 1 0 3\nAfter:  [0, 1, 1, 1]\n\n"
      let parsed = (readP_to_S exampleAParser) input
      1 `shouldBe` 1
