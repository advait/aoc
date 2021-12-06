module TestUtils where

import Control.Monad (unless)
import qualified Data.Text as Text
import Interpreter (eval, evalInterpreter, execInterpreter, initState)
import Parser (Parser, fileP)
import System.IO (hPrint, hPutStrLn, stderr)
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
throwInterpreter int = do
  initState' <- initState
  res <- execInterpreter int initState'
  case res of
    (Left err, state) -> do
      fail $ show err <> "\nState:\n" <> show state
    (Right value, _) -> pure value
