module Interpreter where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExcept, runExceptT)
import qualified Control.Monad.Trans.Except as Except
import Control.Monad.Trans.State (StateT (runStateT), evalStateT)
import qualified Control.Monad.Trans.State as State
import Data.IORef (IORef)
import qualified Data.IORef as IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Env
import GHC.Base (divInt)
import Types
import Prelude hiding (error, lookup)

-- | Evaluates the given expression, yielding a value.
eval :: DExpr -> Interpreter DExpr
-- Integers evaluate to themselves
eval i@(DInt _) = pure i
-- Empty lists evaluate to themselves
eval i@(DList []) = pure i
-- Special form function calls
-- def! binds the name to the given value
eval e@(DList (DSymbol "def!" : params)) = do
  (p1, p2) <- expect2 params
  name <- expectSymbol p1
  value <- eval p2
  _ <- setBinding name value
  pure value
-- let creates a new environment and binds provided names to values in the new environment
eval e@(DList (DSymbol "let" : params)) = do
  (bindings', finalExpr) <- expect2 params
  bindings <- expectList bindings'
  let expectPairs :: [DExpr] -> Interpreter [(String, DExpr)]
      expectPairs (p1 : p2 : tail) = do
        name <- expectSymbol p1
        value <- eval p2
        (:) (name, value) <$> expectPairs tail
      expectPairs [] = pure []
      expectPairs _ = iError (ArgumentCountError 2 (length bindings)) e
  bindingPairs <- expectPairs bindings
  let (names, values) = unzip bindingPairs
  call names values finalExpr
-- Function definition
eval e@(DList (DSymbol "fn" : outerParams)) = do
  _ <- expectFnDef e -- Verify the fn call has the right structure
  pure e
-- General function calls
eval e@(DList (name : params)) = do
  binding <- lookup name
  case binding of
    StaticBinding fn -> do
      (targetNames, finalExpr) <- expectFnDef fn
      evalParams <- sequence (eval <$> params)
      call targetNames evalParams finalExpr
    BuiltinFn f -> do
      evalParams <- sequence (eval <$> params)
      f evalParams
-- Dereference symbols relative to the environment
eval i@(DSymbol s) = do
  binding <- lookup i
  case binding of
    StaticBinding b -> pure b
    BuiltinFn _ -> iError ReferenceError i

-- | Creates a new environemnt, binds the provided values to the given namesl, evaluates the
-- | expression in the new environment, returns the result, and destroys the created environment.
call :: [String] -> [DExpr] -> DExpr -> Interpreter DExpr
call names values expr = do
  unless (length names == length values) (iError (ArgumentCountError (length names) (length values)) expr)
  let bindingPairs = zip names values
  pushEnv
  sequence_ (uncurry setBinding <$> bindingPairs)
  finalValue <- eval expr
  popEnv
  pure finalValue

-- | Bindings for builtin functions and values.
builtins :: IO Env
builtins =
  IORef.newIORef $
    Map.fromList
      [ ("+", BuiltinFn $ fn2IntIntInt (+)),
        ("-", BuiltinFn $ fn2IntIntInt (-)),
        ("*", BuiltinFn $ fn2IntIntInt (*)),
        ("/", BuiltinFn $ fn2IntIntInt divInt)
      ]

initState :: IO IState
initState = do
  builtins' <- builtins
  pure $ IState {envStack = [builtins']}

-- | Runs the interpreter with the provided state, yielding the result and the next state.
execInterpreter :: Interpreter DExpr -> IState -> IO (Either IError (DExpr, IState))
execInterpreter interp state = runExceptT $ runStateT interp state

-- | Runs the interpreter with the initial state, discarding the final state.
evalInterpreter :: Interpreter a -> IO (Either IError a)
evalInterpreter interpreter = do
  initState' <- initState
  runExceptT $ evalStateT interpreter initState'
