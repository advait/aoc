module Interpreter where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (Except, runExcept, throwE)
import Control.Monad.Trans.State (StateT (runStateT), evalStateT)
import qualified Control.Monad.Trans.State as State
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Base (divInt)
import Types
import Prelude hiding (error, lookup)

-- | The core Decent Interpreter which maintains a state IState, can error with IError, and
-- | evaluates to a value v.
type Interpreter v = StateT IState (Except IError) v

newtype IState = IState {envStack :: [Bindings]}

data IError = IError ErrorType DExpr

instance Show IError where
  show (IError errorType expr) = show errorType <> "\nwhile evaluating expression '" <> show expr <> "'"

data ErrorType
  = ArgumentCountError Int Int
  | ReferenceError
  | TypeError DType DType

instance Show ErrorType where
  show (ArgumentCountError expected actual) = "ArgumentCountError (expected: " <> show expected <> ", actual: " <> show actual <> ")"
  show ReferenceError = "ReferenceError"
  show (TypeError expected actual) = "TypeError (expected: " <> show expected <> ", actual: " <> show actual <> ")"

-- | Evaluates the given expression, yielding a value.
eval :: DExpr -> Interpreter DExpr
-- Integers evaluate to themselves
eval i@(DInt _) = pure i
-- Empty lists evaluate to themselves
eval i@(DList []) = pure i
-- Function calls
eval e@(DList (name : params)) = do
  binding <- lookup name
  case binding of
    StaticBinding _ -> iError ReferenceError e
    BuiltinFn f -> do
      evalParams <- sequence (eval <$> params)
      f evalParams
-- Dereference symbols relative to the environment
eval i@(DSymbol s) = do
  binding <- lookup i
  case binding of
    StaticBinding b -> pure b
    BuiltinFn _ -> iError ReferenceError i

data Binding
  = StaticBinding DExpr
  | BuiltinFn ([DExpr] -> Interpreter DExpr)

type Bindings = Map String Binding

rootBindings :: Bindings
rootBindings =
  Map.fromList
    [ ("+", BuiltinFn $ fn2IntIntInt (+)),
      ("-", BuiltinFn $ fn2IntIntInt (-)),
      ("*", BuiltinFn $ fn2IntIntInt (*)),
      ("/", BuiltinFn $ fn2IntIntInt divInt)
    ]

initState :: IState
initState =
  IState
    { envStack = [rootBindings]
    }

-- | Recursively looks up a value in the environment stack.
lookup :: DExpr -> Interpreter Binding
lookup expr = do
  name <- expectSymbol expr
  state <- State.get
  let lookup' :: [Bindings] -> Interpreter Binding
      lookup' [] = iError ReferenceError expr
      lookup' (head : tail) = case Map.lookup name head of
        Nothing -> lookup' tail
        Just binding -> pure binding
  lookup' $ envStack state

-- | Helper to construct and throw errors.
iError :: ErrorType -> DExpr -> Interpreter a
iError errorType expr = lift $ throwE $ IError errorType expr

expectSymbol :: DExpr -> Interpreter String
expectSymbol (DSymbol s) = pure s
expectSymbol e = iError (TypeError TSymbol (typeOf e)) e

expectInt :: DExpr -> Interpreter Int
expectInt (DInt i) = pure i
expectInt e = iError (TypeError TInt (typeOf e)) e

fn2IntIntInt :: (Int -> Int -> Int) -> [DExpr] -> Interpreter DExpr
fn2IntIntInt f [DInt p1, DInt p2] = pure $ DInt $ f p1 p2
fn2IntIntInt _ _ = undefined

execInterpreter :: Interpreter DExpr -> IState -> Either IError (DExpr, IState)
execInterpreter interp state = runExcept $ runStateT interp state

-- | Runs the interpreter with the initial state, discarding the final state.
evalInterpreter :: Interpreter a -> Either IError a
evalInterpreter interpreter = runExcept $ evalStateT interpreter initState
