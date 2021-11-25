module Interpreter where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (Except, runExcept, throwE)
import Control.Monad.Trans.State (StateT (runStateT), evalStateT)
import GHC.Base (divInt)
import Types
import Prelude hiding (error, lookup)

-- | The core Decent Interpreter which maintains a state IState, can error with IError, and
-- | evaluates to a value v.
type Interpreter v = StateT IState (Except IError) v

type IState = ()

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
eval i@(DSymbol s) = do
  binding <- lookup i
  case binding of
    StaticBinding b -> pure b
    BuiltinFn _ -> iError ReferenceError i

iError :: ErrorType -> DExpr -> Interpreter a
iError errorType expr = lift $ throwE $ IError errorType expr

data Binding
  = StaticBinding DExpr
  | BuiltinFn ([DExpr] -> Interpreter DExpr)

-- | Looks up a name in the environment.
lookup' :: String -> Maybe Binding
lookup' "+" = Just $ BuiltinFn $ fn2IntIntInt (+)
lookup' "-" = Just $ BuiltinFn $ fn2IntIntInt (-)
lookup' "*" = Just $ BuiltinFn $ fn2IntIntInt (*)
lookup' "/" = Just $ BuiltinFn $ fn2IntIntInt divInt
lookup' _ = Nothing

lookup :: DExpr -> Interpreter Binding
lookup e@(DSymbol name) = case lookup' name of
  Just b -> pure b
  Nothing -> iError ReferenceError e
lookup e = iError (TypeError TSymbol (typeOf e)) e

expectSymbol :: DExpr -> Interpreter String
expectSymbol (DSymbol s) = pure s
expectSymbol e = iError (TypeError TSymbol (typeOf e)) e

expectInt :: DExpr -> Interpreter Int
expectInt (DInt i) = pure i
expectInt e = iError (TypeError TInt (typeOf e)) e

fn2IntIntInt :: (Int -> Int -> Int) -> [DExpr] -> Interpreter DExpr
fn2IntIntInt f [DInt p1, DInt p2] = pure $ DInt $ f p1 p2
fn2IntIntInt _ _ = undefined

initState :: IState
initState = ()

execInterpreter :: Interpreter DExpr -> IState -> Either IError (DExpr, IState)
execInterpreter interp state = runExcept $ runStateT interp state

-- | Runs the interpreter with the initial state, discarding the final state.
evalInterpreter :: Interpreter a -> Either IError a
evalInterpreter interpreter = runExcept $ evalStateT interpreter initState
