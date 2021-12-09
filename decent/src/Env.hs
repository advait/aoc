module Env where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Trans.State as State
import qualified Data.IORef as IORef
import qualified Data.Map as Map
import GHC.Base (divInt)
import Types
import Prelude hiding (lookup)

-- | Sets the key to the given value in the current environment.
bind :: String -> DExpr -> Interpreter ()
bind key value = do
  s <- getState
  case envStack s of
    [] -> iError EmptyStackError
    (head : tail) -> putState $ s {envStack = Map.insert key (StrictBinding value) head : tail}

-- | Creates a new lazy binding whose evaluation will be deferred until the value is looked up.
bindLazy :: String -> Interpreter DExpr -> Interpreter ()
bindLazy key value = do
  s <- getState
  case envStack s of
    [] -> iError EmptyStackError
    (head : tail) -> do
      ref <- newIORef $ Deferred value
      putState $ s {envStack = Map.insert key (LazyBinding ref) head : tail}

-- | Binds all of the names to the given values in the current environment, handling varargs syntax.
bindAll :: [String] -> [DExpr] -> Interpreter ()
bindAll names values =
  let bindAll' :: [String] -> [DExpr] -> Interpreter ()
      bindAll' [] [] = pure ()
      -- Varargs
      bindAll' ("&" : tail) values = do
        name <- expect1 tail
        bind name $ DList values
      bindAll' (name : nTail) (value : vTail) = do
        bind name value
        bindAll' nTail vTail
      -- TODO: Potential partially bound state if we get an error here.
      bindAll' _ _ = iError $ ArgumentCountError (length names) (length values)
   in bindAll' names values
