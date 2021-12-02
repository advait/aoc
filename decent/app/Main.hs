module Main where

import Control.Monad.Trans.Class (lift)
import Data.List.Extra (trim)
import Data.Version (showVersion)
import Interpreter
import Parser
import Paths_decent (version)
import System.Console.Haskeline
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
      case trim <$> minput of
        Nothing -> return ()
        Just "" -> loop state
        Just "exit" -> return ()
        Just "quit" -> return ()
        Just input -> do
          case parse input of
            Left err -> do
              outputStrLn $ show err
              loop state
            Right expr -> do
              value <- lift $ execInterpreter (eval expr) state
              case value of
                Left err -> do
                  outputStrLn $ show err
                  loop state
                Right (res, state') -> do
                  outputStrLn $ show res
                  loop state'