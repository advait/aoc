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
import qualified Data.Text as Text
import Text.Parsec (ParsecT)
import qualified Text.Parsec as Parsec

-- | The core Decent Interpreter which maintains a state IState, can error with IError, and
-- | evaluates to a value v.
type Interpreter v = StateT IState (ExceptT IError IO) v

-- | The interpreter state.
data IState = IState
  { -- Bindings are stored in nested environments.
    envStack :: [Env],
    -- Expressions are evaluated recursively, stored on this stack for better error tracing.
    exprStack :: [DExpr],
    -- The directory relative to which we are importing other files.
    importDir :: FilePath
  }

-- | An environment is a mapping from names to expressions.
type Env = IORef (Map String DExpr)

-- | An interpreter error.
data IError = IError ErrorType [DExpr]

instance Show IError where
  show (IError errorType exprStack) = unlines [show errorType, "while evaluating expression:", showStack]
    where
      showStack = unlines ((<>) "  " . show <$> exprStack)

data ErrorType
  = ArgumentCountError Int Int
  | ReferenceError String
  | TypeError DType DType
  | EmptyStackError
  | SyntaxError String
  | RuntimeError String

instance Show ErrorType where
  show (ArgumentCountError expected actual) = "ArgumentCountError (expected: " <> show expected <> ", actual: " <> show actual <> ")"
  show (ReferenceError name) = "ReferenceError (" <> show name <> ")"
  show (TypeError expected actual) = "TypeError (expected: " <> show expected <> ", actual: " <> show actual <> ")"
  show EmptyStackError = "EmptyStackError (no stack frames for binding new value)"
  show (SyntaxError info) = "SyntaxError (" <> info <> ")"
  show (RuntimeError info) = "RuntimeError (" <> info <> ")"

-- | An expression that can be evaluated.
data DExpr
  = DSymbol String
  | DInt Int
  | DString String
  | DChar Char
  | DList [DExpr]
  | DFunction ([DExpr] -> Interpreter DExpr)

instance Show DExpr where
  show (DSymbol symbol) = symbol
  show (DInt int) = show int
  show (DString s) = show s
  show (DChar c) = show c
  show (DList l) = "(" <> unwords (show <$> l) <> ")"
  show (DFunction _) = "#<function>"

instance Eq DExpr where
  (DSymbol a) == (DSymbol b) = a == b
  (DInt a) == (DInt b) = a == b
  (DString a) == (DString b) = a == b
  (DChar a) == (DChar b) = a == b
  (DList a) == (DList b) = a == b
  _ == _ = False

data DType
  = TSymbol
  | TInt
  | TString
  | TChar
  | TList
  | TFunction

instance Show DType where
  show TSymbol = "Symbol"
  show TInt = "Int"
  show TString = "String"
  show TChar = "Char"
  show TList = "List"
  show TFunction = "Function"

typeOf :: DExpr -> DType
typeOf (DSymbol _) = TSymbol
typeOf (DInt _) = TInt
typeOf (DString _) = TString
typeOf (DChar _) = TChar
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
popExpr = StateT.modify $ \s -> s {exprStack = tail $ exprStack s}

getCallStack :: Interpreter [DExpr]
getCallStack = exprStack <$> StateT.get

getCurExpr :: Interpreter DExpr
getCurExpr = head <$> getCallStack

getImportDir :: Interpreter FilePath
getImportDir = importDir <$> StateT.get

setImportDir :: FilePath -> Interpreter ()
setImportDir dir = StateT.modify $ \s -> s {importDir = dir}

-- Convenience expression assertions

expectSymbol :: DExpr -> Interpreter String
expectSymbol (DSymbol s) = pure s
expectSymbol e = iError $ TypeError TSymbol $ typeOf e

expectInt :: DExpr -> Interpreter Int
expectInt (DInt i) = pure i
expectInt e = iError $ TypeError TInt $ typeOf e

expectString :: DExpr -> Interpreter String
expectString (DString s) = pure s
expectString e = iError $ TypeError TString $ typeOf e

expect1 :: [a] -> Interpreter a
expect1 [p1] = pure p1
expect1 l = iError $ ArgumentCountError 1 $ length l

expect2 :: [a] -> Interpreter (a, a)
expect2 [p1, p2] = pure (p1, p2)
expect2 l = iError $ ArgumentCountError 2 $ length l

expect3 :: [a] -> Interpreter (a, a, a)
expect3 [p1, p2, p3] = pure (p1, p2, p3)
expect3 l = iError $ ArgumentCountError 3 $ length l

expectList :: DExpr -> Interpreter [DExpr]
expectList (DList l) = pure l
expectList e = iError $ TypeError TList $ typeOf e

expectFnDef :: [DExpr] -> Interpreter ([String], DExpr)
expectFnDef params = do
  (p1, p2) <- expect2 params
  params' <- expectList p1
  params <- sequence (expectSymbol <$> params')
  pure (params, p2)

fn2IntIntInt :: (Int -> Int -> Int) -> DExpr
fn2IntIntInt f =
  DFunction $ \params -> do
    ints <- sequence $ expectInt <$> params
    (p1, p2) <- expect2 ints
    pure $ DInt $ f p1 p2
