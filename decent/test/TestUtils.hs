module TestUtils where

import Control.Monad (unless)
import qualified Data.Text as Text
import Parser
import Test.Hspec
import Test.QuickCheck
import Text.Parsec
import Types

-- | Converts Either into IO, throwing if we get an error.
throwEither :: Show err => Either err b -> IO b
throwEither (Left err) = fail $ show err
throwEither (Right b) = pure b

-- | Runs the parser on the input, throwing if we get an error.
throwParser :: Parser a -> String -> IO a
throwParser parser input =
  throwEither $ runParser parser () "(source)" (Text.pack input)
