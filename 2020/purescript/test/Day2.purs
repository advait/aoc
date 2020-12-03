module Test.Day2 where

import Day2
import Prelude
import Data.Maybe (Maybe(..))
import Parser (runParser)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Day 2" do
    describe "xor" do
      it "handles false false" $ xor false false `shouldEqual` false
      it "handles false true" $ xor false true `shouldEqual` true
      it "handles true false" $ xor true false `shouldEqual` true
      it "handles true true" $ xor true true `shouldEqual` false
    describe "Part B" do
      let
        parse s = (\foo -> foo.p) <$> runParser passwordItemParser s

        p1 = parse "1-3 a: abcde"

        p2 = parse "1-3 b: cdefg"

        p3 = parse "2-9 c: ccccccccc"

        input = [ 1721, 979, 366, 299, 675, 1456 ]
      it "solves p1" $ (validPartB <$> p1) `shouldEqual` Just true
      it "solves p2" $ (validPartB <$> p2) `shouldEqual` Just false
      it "solves p3" $ (validPartB <$> p3) `shouldEqual` Just false
