module Advent16Spec where

import A16.Advent16
import           Test.Hspec
import           Text.ParserCombinators.ReadP

spec :: Spec
spec =
  describe "Parse Example A" $
  it "parses successfully" $ do
    let input = "Before: [0, 1, 1, 0]\n7 1 0 3\nAfter:  [0, 1, 1, 1]\n\n"
    let parsed = readP_to_S exampleAParser input
    length parsed `shouldBe` 1
    let (ea, remaining) = head parsed
    beforeRegs ea `shouldBe` (0, 1, 1, 0)
    opNum ea `shouldBe` 7
    params ea `shouldBe` (1, 0, 3)
    afterRegs ea `shouldBe` (0, 1, 1, 1)
