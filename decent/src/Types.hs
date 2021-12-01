module Types where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.Except as Except
import Control.Monad.Trans.State (StateT)
import Data.IORef (IORef)
import qualified Data.IORef as IORef
import Data.Map (Map)

-- | The core Decent Interpreter which maintains a state IState, can error with IError, and
-- | evaluates to a value v.
type Interpreter v = StateT IState (ExceptT IError IO) v

data Binding
  = StaticBinding DExpr
  | BuiltinFn ([DExpr] -> Interpreter DExpr)

type Env = IORef (Map String Binding)

newtype IState = IState {envStack :: [Env]}

data IError = IError ErrorType DExpr

instance Show IError where
  show (IError errorType expr) = show errorType <> "\nwhile evaluating expression '" <> show expr <> "'"

data ErrorType
  = ArgumentCountError Int Int
  | ReferenceError
  | TypeError DType DType
  | EmptyStackError
  | SyntaxError String String

instance Show ErrorType where
  show (ArgumentCountError expected actual) = "ArgumentCountError (expected: " <> show expected <> ", actual: " <> show actual <> ")"
  show ReferenceError = "ReferenceError"
  show (TypeError expected actual) = "TypeError (expected: " <> show expected <> ", actual: " <> show actual <> ")"
  show EmptyStackError = "EmptyStackError (no stack frames for binding new value)"
  show (SyntaxError expected actual) = "SyntaxError (expected: " <> show expected <> ", actual: " <> show actual <> ")"

-- | An expression that can be evaluated.
data DExpr
  = DSymbol String
  | DInt Int
  | DList [DExpr] -- A DList is a lisp S-Expression
  deriving (Eq)

instance Show DExpr where
  show (DSymbol symbol) = symbol
  show (DInt int) = show int
  show (DList l) = "(" <> unwords (show <$> l) <> ")"

data DType = TSymbol | TInt | TList
  deriving (Show)

typeOf :: DExpr -> DType
typeOf (DSymbol _) = TSymbol
typeOf (DInt _) = TInt
typeOf (DList _) = TList

-- Convenience lifted functions

newIORef :: a -> Interpreter (IORef a)
newIORef = liftIO . IORef.newIORef

readIORef :: IORef a -> Interpreter a
readIORef = liftIO . IORef.readIORef

writeIORef :: IORef a -> a -> Interpreter ()
writeIORef ref = liftIO . IORef.writeIORef ref

throwE :: IError -> Interpreter a
throwE e = lift $ Except.throwE e

-- | Helper to construct and throw errors.
iError :: ErrorType -> DExpr -> Interpreter a
iError errorType expr = throwE $ IError errorType expr

-- Convenience expression assertions

expectSymbol :: DExpr -> Interpreter String
expectSymbol (DSymbol s) = pure s
expectSymbol e = iError (TypeError TSymbol (typeOf e)) e

expectInt :: DExpr -> Interpreter Int
expectInt (DInt i) = pure i
expectInt e = iError (TypeError TInt (typeOf e)) e

expect2 :: [a] -> Interpreter (a, a)
expect2 [p1, p2] = pure (p1, p2)
expect2 l = iError (ArgumentCountError 2 (length l)) undefined

expect3 :: [a] -> Interpreter (a, a, a)
expect3 [p1, p2, p3] = pure (p1, p2, p3)
expect3 l = iError (ArgumentCountError 3 (length l)) undefined

expectList :: DExpr -> Interpreter [DExpr]
expectList (DList l) = pure l
expectList e = iError (TypeError TList (typeOf e)) e

expectFnDef :: DExpr -> Interpreter ([String], DExpr)
expectFnDef expr = do
  (p1, p2, p3) <- expectList expr >>= expect3
  fnName <- expectSymbol p1
  unless (fnName == "fn") (iError (SyntaxError "fn" fnName) expr)
  params' <- expectList p2
  params <- sequence (expectSymbol <$> params')
  pure (params, p3)

expectFn2 :: DExpr -> Interpreter (DExpr, DExpr)
expectFn2 (DList [p1, p2]) = pure (p1, p2)
expectFn2 e@(DList l) = iError (ArgumentCountError 2 (length l)) e
expectFn2 e = iError (TypeError TList (typeOf e)) e
