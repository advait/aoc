module Day2 where

import Parser
import Prelude
import Data.Array (filter, length)
import Data.Char.Unicode (isAlpha)
import Data.Maybe (Maybe(..))
import Data.String (uncons)
import Data.String.CodeUnits (charAt)
import Effect (Effect)
import Effect.Console (logShow)

type PasswordItem
  = { min :: Int
    , max :: Int
    , c :: Char
    , password :: String
    }

passwordItemParser :: Parser PasswordItem
passwordItemParser =
  let
    f min max c password = { min, max, c, password }
  in
    pure f
      <*> intParser
      <* charP '-'
      <*> intParser
      <* charP ' '
      <*> predCharP isAlpha
      <* stringP ": "
      <*> takeWhileCharP isAlpha

inputParser :: Parser (Array PasswordItem)
inputParser = delimitedBy (stringP "\n") passwordItemParser

validPartA :: PasswordItem -> Boolean
validPartA p =
  let
    occurrances :: Char -> String -> Int
    occurrances needle haystack = case uncons haystack of
      Nothing -> 0
      Just { head, tail }
        | needle == (charFromCodePoint head) -> 1 + occurrances needle tail
      Just { head, tail } -> occurrances needle tail

    n = occurrances p.c p.password
  in
    n >= p.min && n <= p.max

partA :: Array PasswordItem -> Int
partA = length <<< filter validPartA

xor :: Boolean -> Boolean -> Boolean
xor a b = (a && not b) || (not a && b)

validPartB :: PasswordItem -> Boolean
validPartB p =
  xor
    (charAt (p.min - 1) p.password == Just p.c)
    (charAt (p.max - 1) p.password == Just p.c)

main :: Effect Unit
main = do
  passwords <- parsePuzzleInput "inputs/2.txt" inputParser
  logShow $ length $ filter validPartA passwords
  logShow $ length $ filter validPartB passwords
