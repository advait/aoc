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
-- Functions evaluate to themselves
eval i@DFunction {} = pure i
-- Special form function calls
-- def! binds the name to the given value
eval e@(DList (DSymbol "def!" : params)) = do
  (p1, p2) <- expect2 params
  name <- expectSymbol p1
  value <- eval p2
  bind name value
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
  pushEnv
  let (names, values) = unzip bindingPairs
  bindAll names values
  finalValue <- eval finalExpr
  popEnv
  pure finalValue
-- Function definition
eval e@(DList (DSymbol "fn" : outerParams)) = do
  (paramNames, result) <- expectFnDef e
  makeClosure paramNames result
-- do statement
eval e@(DList (DSymbol "do" : params)) = do
  let loop [expr] = eval expr
      loop (head : tail) = do
        eval head
        loop tail
      loop [] = iError (ArgumentCountError 1 (length params)) e
  loop params
-- if statement
eval e@(DList (DSymbol "if" : params)) = do
  (p1, p2, p3) <- expect3 params
  v1 <- eval p1
  case v1 of
    e
      | e == dNil -> eval p3
      | e == dFalse -> eval p3
      | otherwise -> eval p2
-- typeof
eval e@(DList (DSymbol "typeof" : params)) = do
  p1 <- expect1 params
  pure $ DSymbol $ show $ typeOf p1
-- list
eval e@(DList (DSymbol "list" : params)) = pure $ DList params
-- General function calls
eval e@(DList (first : params)) = do
  first' <- eval first
  case first' of
    DFunction fn -> do
      evalParams <- sequence (eval <$> params)
      fn evalParams
    e ->
      iError (TypeError TFunction (typeOf e)) e
-- Names dereference in the environment
eval i@(DSymbol s) = lookup i

-- | A closure is created whenever a function is defined. It consists of three things:
-- | 1. The current environment that the function body can refer to.
-- | 2. A list of parameters
-- | 3. An expression to be evaluated when the function is called
makeClosure :: [String] -> DExpr -> Interpreter DExpr
makeClosure names value = do
  closedEnv <- getEnv
  pure $
    DFunction
      ( \params -> do
          -- Temporarily save our existing environment so we can replace it after the function call
          tempEnv <- getEnv
          setEnv closedEnv
          pushEnv
          bindAll names params
          res <- eval value
          setEnv tempEnv
          pure res
      )

-- | Bindings for builtin functions and values.
builtins :: IO Env
builtins =
  IORef.newIORef $
    Map.fromList
      [ ("nil", dNil),
        ("true", dTrue),
        ("false", dFalse),
        ("+", fn2IntIntInt (+)),
        ("-", fn2IntIntInt (-)),
        ("*", fn2IntIntInt (*)),
        ("/", fn2IntIntInt divInt),
        ( "=",
          DFunction $ \params -> do
            (p1, p2) <- expect2 params
            pure $ toDBool $ p1 == p2
        )
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
