module Env where

import Control.Monad (unless)
import qualified Control.Monad.Trans.State as State
import qualified Data.IORef as IORef
import qualified Data.Map as Map
import GHC.Base (divInt)
import Types

-- | Creates a new environment (for function calls).
pushEnv :: Interpreter ()
pushEnv = do
  newEnv <- newIORef mempty
  State.modify (\s -> s {envStack = newEnv : envStack s})

-- | Deletes the current environment.
popEnv :: Interpreter ()
popEnv = do
  env <- getEnv
  case env of
    [] -> iError EmptyStackError undefined
    head : tail -> setEnv tail

-- | Recursively looks up a value in the environment stack.
lookup :: DExpr -> Interpreter DExpr
lookup expr = do
  name <- expectSymbol expr
  env <- getEnv
  let lookup' :: [Env] -> Interpreter DExpr
      lookup' [] = iError ReferenceError expr
      lookup' (head : tail) = do
        env <- readIORef head
        case Map.lookup name env of
          Nothing -> lookup' tail
          Just expr -> pure expr
  lookup' env

-- | Sets the key to the given value in the current environment.
setBinding :: String -> DExpr -> Interpreter ()
setBinding key value = do
  let putBinding :: [Env] -> Interpreter ()
      putBinding [] = iError EmptyStackError value
      putBinding (head : tail) = do
        env <- readIORef head
        let env' = Map.insert key value env
        writeIORef head env'
  env <- getEnv
  putBinding env

-- | Binds all of the names to the given values in the current environment.
bindAll :: [String] -> [DExpr] -> Interpreter ()
bindAll names values = do
  unless (length names == length values) (iError (ArgumentCountError (length names) (length values)) undefined)
  let bindingPairs = zip names values
  sequence_ (uncurry setBinding <$> bindingPairs)
