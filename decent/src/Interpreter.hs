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
eval expr = do
  pushExpr expr
  value <- eval'
  popExpr
  pure value

-- | Internal eval', using the getCurExpr
eval' :: Interpreter DExpr
eval' = do
  expr <- getCurExpr
  case expr of
    -- Integers evaluate to themselves
    i@(DInt _) -> pure i
    -- Empty lists evaluate to themselves
    i@(DList []) -> pure i
    -- Functions evaluate to themselves
    i@DFunction {} -> pure i
    -- Names dereference in the environment
    i@(DSymbol _) -> lookup i
    -- Named function calls
    DList (DSymbol name : params) ->
      special name params
    -- Unnamed function calls
    DList (p1 : params) -> general p1 params
  where
    -- special forms (like "if" and "fn") that require unique evaluation
    special :: String -> [DExpr] -> Interpreter DExpr
    -- def! binds the name to the given value
    special "def" params = do
      (p1, p2) <- expect2 params
      name <- expectSymbol p1
      value <- eval p2
      bind name value
      pure value

    -- let creates a new environmen,t binds provided names to values in the new environment,
    -- evaluates the final expression in the context of the environment, and discards the env.
    special "let" params = do
      (bindings', finalExpr) <- expect2 params
      bindings <- expectList bindings'
      let expectPairs :: [DExpr] -> Interpreter [(String, DExpr)]
          expectPairs (p1 : p2 : tail) = do
            name <- expectSymbol p1
            value <- eval p2
            (:) (name, value) <$> expectPairs tail
          expectPairs [] = pure []
          expectPairs _ = iError (ArgumentCountError 2 (length bindings))
      bindingPairs <- expectPairs bindings
      pushEnv
      let (names, values) = unzip bindingPairs
      bindAll names values
      finalValue <- eval finalExpr
      popEnv
      pure finalValue

    -- Function definition
    special "fn" _ = do
      e <- getCurExpr
      (paramNames, result) <- expectFnDef e
      makeClosure paramNames result

    -- do statement
    special "do" params = do
      let loop [expr] = eval expr
          loop (head : tail) = do
            eval head
            loop tail
          loop [] = iError (ArgumentCountError 1 (length params))
      loop params

    -- if statement
    special "if" params = do
      (p1, p2, p3) <- expect3 params
      v1 <- eval p1
      case v1 of
        e
          | e == dNil -> eval p3
          | e == dFalse -> eval p3
          | otherwise -> eval p2

    -- typeof
    special "typeof" params = do
      p1 <- expect1 params
      pure $ DSymbol $ show $ typeOf p1

    -- list
    special "list" params = pure $ DList params
    -- General function calls
    special first params = general (DSymbol first) params

    -- General function calls
    general first params = do
      first' <- eval first
      case first' of
        DFunction fn -> do
          evalParams <- sequence (eval <$> params)
          fn evalParams
        e ->
          iError (TypeError TFunction (typeOf e))

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
  pure $ IState {envStack = [builtins'], exprStack = []}

-- | Runs the interpreter with the provided state, yielding the result and the next state.
execInterpreter :: Interpreter DExpr -> IState -> IO (Either IError (DExpr, IState))
execInterpreter interp state = runExceptT $ runStateT interp state

-- | Runs the interpreter with the initial state, discarding the final state.
evalInterpreter :: Interpreter a -> IO (Either IError a)
evalInterpreter interpreter = do
  initState' <- initState
  runExceptT $ evalStateT interpreter initState'
