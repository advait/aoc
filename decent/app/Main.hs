module Main where

import Control.Monad.Trans.Class (lift)
import Data.List.Extra (trim)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as Text
import Data.Version (showVersion)
import Interpreter (eval, execInterpreter, initState)
import Parser (replLineP)
import Paths_decent (version)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStrLn, runInputT)
import qualified Text.Parsec as Parsec
import Types

main :: IO ()
main = do
  putStrLn $ "decent repl " <> showVersion version
  repl

repl :: IO ()
repl = do
  initState' <- initState
  runInputT defaultSettings (loop initState')
  where
    loop :: IState -> InputT IO ()
    loop state = do
      minput <- getInputLine "λ>> "
      case minput of
        Nothing -> pure ()
        Just line ->
          case Parsec.parse replLineP "" (Text.pack line) of
            Left err -> do
              outputStrLn $ show err
              loop state
            Right Nothing -> loop state
            Right (Just expr) -> do
              value <- lift $ execInterpreter (eval expr) state
              case value of
                Left err -> do
                  outputStrLn $ show err
                  loop state
                Right (res, state') -> do
                  outputStrLn $ show res
                  loop state'
