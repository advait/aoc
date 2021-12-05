module TestUtils where

import Control.Monad (unless)
import qualified Data.Text as Text
import Interpreter (eval, evalInterpreter)
import Parser (Parser, fileP)
import Text.Parsec (runParser)
import qualified Text.Parsec as Parsec
import Types (Interpreter)

-- | Converts Either into IO, throwing if we get an error.
throwEither :: Show err => Either err b -> IO b
throwEither (Left err) = fail $ show err
throwEither (Right b) = pure b

-- | Runs the parser on the input, throwing if we get an error.
throwParser :: Parser a -> String -> IO a
throwParser parser input =
  throwEither $ runParser parser () "" (Text.pack input)

-- | Runs the interpreter, throwing if we get an error.
throwInterpreter :: Interpreter a -> IO a
throwInterpreter int = evalInterpreter int >>= throwEither
