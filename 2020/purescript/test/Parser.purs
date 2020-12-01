module Test.Parser where

import Parser
import Prelude
import Data.Int (Radix, decimal, toStringAs)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (codePointFromChar, dropWhile, joinWith, stripPrefix)
import Data.String.CodeUnits (singleton)
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import Test.QuickCheck ((===))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)

spec :: Spec Unit
spec =
  describe "charP" do
    it "parses when c is prefixed"
      $ quickCheck \(Tuple c suffix) ->
          let
            testS = (singleton c) <> suffix
          in
            runParser (charP c) testS === Just { next: suffix, p: c }
    it "does not parse when c is not prefixed"
      $ quickCheck \(Tuple c suffix) ->
          let
            testS = dropWhile ((==) (codePointFromChar c)) suffix
          in
            runParser (charP c) testS === Nothing
    describe "stringP" do
      it "parses when s is prefixed"
        $ quickCheck \(Tuple prefix suffix) ->
            runParser (stringP prefix) (prefix <> suffix) === Just { next: suffix, p: prefix }
      it "does not parse when s is not prefixed"
        $ quickCheck \(Tuple prefix suffix) ->
            let
              testS :: String
              testS = (fromMaybe suffix) $ stripPrefix (Pattern prefix) suffix
            in
              runParser (stringP prefix) testS
                === if prefix == "" then
                    Just { next: testS, p: "" }
                  else
                    Nothing
    describe "delimitedBy" do
      it "parses newline delimited items" do
        let
          items = [ "a", "a", "a" ]
        let
          testS = joinWith "\n" items <> "\n"
        let
          parser = delimitedBy (stringP "\n") (stringP "a")
        runParser parser testS `shouldEqual` Just { next: "", p: items }
    describe "intParser" do
      it "successfully parses valid integers"
        $ quickCheck \(Tuple int suffix) ->
            runParser intParser ((toStringAs decimal int) <> suffix) === Just { next: suffix, p: int }
