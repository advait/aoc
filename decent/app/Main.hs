module Main where

import Data.List.Extra (trim)
import Interpreter
import Parser
import System.Console.Haskeline

main :: IO ()
main = do
  putStrLn "decent repl 0.0.1"
  repl

repl :: IO ()
repl = runInputT defaultSettings (loop initState)
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
              case execInterpreter (eval expr) state of
                Left err -> do
                  outputStrLn $ show err
                  loop state
                Right (res, state') -> do
                  outputStrLn $ show res
                  loop state'
