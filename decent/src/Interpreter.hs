module Interpreter where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (Except, runExcept, throwE)
import Control.Monad.Trans.State (StateT (runStateT), evalStateT)
import GHC.Base (divInt)
import Types (DExpr (DInt, DList, DSymbol))
import Prelude hiding (lookup)

-- | The core Decent Interpreter which maintains a state IState, can error with IError, and
-- | evaluates to a value v.
type Interpreter v = StateT IState (Except IError) v

type IState = ()

type IError = ()

-- | Evaluates the given expression, yielding a value.
eval :: DExpr -> Interpreter DExpr
-- Integers evaluate to themselves
eval i@(DInt _) = pure i
-- Empty lists evaluate to themselves
eval i@(DList []) = pure i
-- Function calls
eval (DList (head : params)) = do
  name <- expectSymbol head
  binding <- lookup name
  case binding of
    StaticBinding _ -> lift $ throwE ()
    BuiltinFn f -> do
      evalParams <- sequence (eval <$> params)
      f evalParams

-- All other forms of evaluation fail
eval _ = lift $ throwE ()

data Binding
  = StaticBinding DExpr
  | BuiltinFn ([DExpr] -> Interpreter DExpr)

-- | Looks up a name in the environment.
lookup :: String -> Interpreter Binding
lookup "+" = pure $ BuiltinFn $ fn2IntIntInt (+)
lookup "-" = pure $ BuiltinFn $ fn2IntIntInt (-)
lookup "*" = pure $ BuiltinFn $ fn2IntIntInt (*)
lookup "/" = pure $ BuiltinFn $ fn2IntIntInt divInt
lookup _ = lift $ throwE ()

expectSymbol :: DExpr -> Interpreter String
expectSymbol (DSymbol s) = pure s
expectSymbol _ = lift $ throwE ()

expectInt :: DExpr -> Interpreter Int
expectInt (DInt i) = pure i
expectInt _ = lift $ throwE ()

fn2IntIntInt :: (Int -> Int -> Int) -> [DExpr] -> Interpreter DExpr
fn2IntIntInt f [DInt p1, DInt p2] = pure $ DInt $ f p1 p2
fn2IntIntInt _ _ = lift $ throwE ()

initState :: IState
initState = ()

-- | Evaluates the interpreter.
execInterpreter :: Interpreter a -> Either IError a
execInterpreter interpreter = runExcept $ evalStateT interpreter initState
