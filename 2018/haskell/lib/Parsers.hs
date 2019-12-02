module Parsers where

import           Text.ParserCombinators.ReadP

isDigit c = c >= '0' && c <= '9'

intReader :: ReadP Int
intReader = do
  digits <- many1 $ satisfy isDigit
  return $ read digits

repeatedNReader :: Int -> String -> ReadP a -> ReadP [a]
repeatedNReader n sep del =
  count
    n
    (do ret <- del
        _ <- optional $ string sep
        return ret)

