module Interpreter where

import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExcept, runExceptT)
import qualified Control.Monad.Trans.Except as Except
import Control.Monad.Trans.State (StateT (runStateT), evalStateT)
import qualified Control.Monad.Trans.State as State
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Base (divInt)
import Types
import Prelude hiding (error, lookup)

-- | The core Decent Interpreter which maintains a state IState, can error with IError, and
-- | evaluates to a value v.
type Interpreter v = StateT IState (ExceptT IError IO) v

newtype IState = IState {envStack :: [Bindings]}

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

-- | Creates a new environment (for function calls).
pushEnv :: Interpreter ()
pushEnv = State.modify (\s -> s {envStack = mempty : envStack s})

-- | Deletes the current environment.
popEnv :: Interpreter ()
popEnv = do
  state <- State.get
  case envStack state of
    [] -> iError EmptyStackError undefined
    head : tail -> State.put $ state {envStack = tail}

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

-- | Sets the key to the given value in the current environment stack.
setBinding :: String -> DExpr -> Interpreter ()
setBinding key value = do
  let putBinding :: [Bindings] -> Interpreter [Bindings]
      putBinding [] = iError EmptyStackError value
      putBinding (head : tail) = pure $ Map.insert key (StaticBinding value) head : tail
  state <- State.get
  envStack' <- putBinding (envStack state)
  State.put $ state {envStack = envStack'}

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

-- | Helper to construct and throw errors.
iError :: ErrorType -> DExpr -> Interpreter a
iError errorType expr = lift $ Except.throwE $ IError errorType expr

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

fn2IntIntInt :: (Int -> Int -> Int) -> [DExpr] -> Interpreter DExpr
fn2IntIntInt f [DInt p1, DInt p2] = pure $ DInt $ f p1 p2
fn2IntIntInt _ params = iError (ArgumentCountError 2 (length params)) undefined

execInterpreter :: Interpreter DExpr -> IState -> IO (Either IError (DExpr, IState))
execInterpreter interp state = runExceptT $ runStateT interp state

-- | Runs the interpreter with the initial state, discarding the final state.
evalInterpreter :: Interpreter a -> IO (Either IError a)
evalInterpreter interpreter = runExceptT $ evalStateT interpreter initState
