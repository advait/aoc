module Types where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.Except as Except
import Control.Monad.Trans.State (StateT)
import qualified Control.Monad.Trans.State as StateT
import Data.Functor.Identity (Identity)
import Data.IORef (IORef)
import qualified Data.IORef as IORef
import Data.Map (Map)
import Data.Text (Text)
import Text.Parsec (ParsecT)

-- | The core Decent Interpreter which maintains a state IState, can error with IError, and
-- | evaluates to a value v.
type Interpreter v = StateT IState (ExceptT IError IO) v

data IState = IState
  { envStack :: [Env],
    exprStack :: [DExpr]
  }

type Env = IORef (Map String DExpr)

data IError = IError ErrorType [DExpr]

instance Show IError where
  show (IError errorType exprStack) = show errorType <> "\nwhile evaluating expression:\n" <> showStack
    where
      showStack = unlines ((<>) "  " . show <$> exprStack)

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
  | DFunction ([DExpr] -> Interpreter DExpr)

instance Show DExpr where
  show (DSymbol symbol) = symbol
  show (DInt int) = show int
  show (DList l) = "(" <> unwords (show <$> l) <> ")"
  show (DFunction _) = "#<function>"

instance Eq DExpr where
  (DSymbol a) == (DSymbol b) = a == b
  (DInt a) == (DInt b) = a == b
  (DList a) == (DList b) = a == b
  _ == _ = False

data DType = TSymbol | TInt | TList | TFunction

instance Show DType where
  show TSymbol = "Symbol"
  show TInt = "Int"
  show TList = "List"
  show TFunction = "Function"

typeOf :: DExpr -> DType
typeOf (DSymbol _) = TSymbol
typeOf (DInt _) = TInt
typeOf (DList _) = TList
typeOf (DFunction _) = TFunction

-- Special values (you may regret keeping these as symbols...)
dTrue :: DExpr
dTrue = DSymbol "true"

dFalse :: DExpr
dFalse = DSymbol "false"

toDBool :: Bool -> DExpr
toDBool True = dTrue
toDBool False = dFalse

dNil :: DExpr
dNil = DList []

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
iError :: ErrorType -> Interpreter a
iError errorType = do
  callStack <- getCallStack
  throwE $ IError errorType callStack

getEnv :: Interpreter [Env]
getEnv = envStack <$> StateT.get

setEnv :: [Env] -> Interpreter ()
setEnv envStack = do
  s <- StateT.get
  let s' = s {envStack = envStack}
  StateT.put s'

pushExpr :: DExpr -> Interpreter ()
pushExpr expr = do
  s <- StateT.get
  let s' = s {exprStack = expr : exprStack s}
  StateT.put s'

popExpr :: Interpreter ()
popExpr = do
  s <- StateT.get
  let s' = s {exprStack = tail $ exprStack s}
  StateT.put s'

getCallStack :: Interpreter [DExpr]
getCallStack = exprStack <$> StateT.get

getCurExpr :: Interpreter DExpr
getCurExpr = head <$> getCallStack

-- Convenience expression assertions

expectSymbol :: DExpr -> Interpreter String
expectSymbol (DSymbol s) = pure s
expectSymbol e = iError (TypeError TSymbol (typeOf e))

expectInt :: DExpr -> Interpreter Int
expectInt (DInt i) = pure i
expectInt e = iError (TypeError TInt (typeOf e))

expect1 :: [a] -> Interpreter a
expect1 [p1] = pure p1
expect1 l = iError (ArgumentCountError 1 (length l))

expect2 :: [a] -> Interpreter (a, a)
expect2 [p1, p2] = pure (p1, p2)
expect2 l = iError (ArgumentCountError 2 (length l))

expect3 :: [a] -> Interpreter (a, a, a)
expect3 [p1, p2, p3] = pure (p1, p2, p3)
expect3 l = iError (ArgumentCountError 3 (length l))

expectList :: DExpr -> Interpreter [DExpr]
expectList (DList l) = pure l
expectList e = iError (TypeError TList (typeOf e))

expectFnDef :: DExpr -> Interpreter ([String], DExpr)
expectFnDef expr = do
  (p1, p2, p3) <- expectList expr >>= expect3
  fnName <- expectSymbol p1
  unless (fnName == "fn") (iError (SyntaxError "fn" fnName))
  params' <- expectList p2
  params <- sequence (expectSymbol <$> params')
  pure (params, p3)

expectFn2 :: DExpr -> Interpreter (DExpr, DExpr)
expectFn2 (DList [p1, p2]) = pure (p1, p2)
expectFn2 e@(DList l) = iError (ArgumentCountError 2 (length l))
expectFn2 e = iError (TypeError TList (typeOf e))

fn2IntIntInt :: (Int -> Int -> Int) -> DExpr
fn2IntIntInt f =
  DFunction
    ( \params -> do
        ints <- sequence $ expectInt <$> params
        (p1, p2) <- expect2 ints
        pure $ DInt $ f p1 p2
    )
