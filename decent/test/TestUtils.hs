module TestUtils where

import Control.Monad (unless)
import qualified Data.Text as Text
import Parser (Parser)
import Text.Parsec (runParser)

-- | Converts Either into IO, throwing if we get an error.
throwEither :: Show err => Either err b -> IO b
throwEither (Left err) = fail $ show err
throwEither (Right b) = pure b

-- | Runs the parser on the input, throwing if we get an error.
throwParser :: Parser a -> String -> IO a
throwParser parser input =
  throwEither $ runParser parser () "" (Text.pack input)
