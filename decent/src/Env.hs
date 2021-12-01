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
  state <- State.get
  case envStack state of
    [] -> iError EmptyStackError undefined
    head : tail -> State.put $ state {envStack = tail}

-- | Recursively looks up a value in the environment stack.
lookup :: DExpr -> Interpreter Binding
lookup expr = do
  name <- expectSymbol expr
  state <- State.get
  let lookup' :: [Env] -> Interpreter Binding
      lookup' [] = iError ReferenceError expr
      lookup' (head : tail) = do
        env <- readIORef head
        case Map.lookup name env of
          Nothing -> lookup' tail
          Just binding -> pure binding
  lookup' $ envStack state

-- | Sets the key to the given value in the current environment stack.
setBinding :: String -> DExpr -> Interpreter ()
setBinding key value = do
  let putBinding :: [Env] -> Interpreter ()
      putBinding [] = iError EmptyStackError value
      putBinding (head : tail) = do
        env <- readIORef head
        let env' = Map.insert key (StaticBinding value) env
        writeIORef head env'
  state <- State.get
  putBinding (envStack state)
  pure ()

fn2IntIntInt :: (Int -> Int -> Int) -> [DExpr] -> Interpreter DExpr
fn2IntIntInt f [DInt p1, DInt p2] = pure $ DInt $ f p1 p2
fn2IntIntInt _ params = iError (ArgumentCountError 2 (length params)) undefined
