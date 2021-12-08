{-# LANGUAGE RecursiveDo #-}

module Interpreter where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExcept, runExceptT)
import qualified Control.Monad.Trans.Except as Except
import Control.Monad.Trans.State.Lazy (StateT (runStateT), evalStateT)
import qualified Control.Monad.Trans.State.Lazy as State
import Data.IORef (IORef)
import qualified Data.IORef as IORef
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Env (bind, bindAll, bindLazy)
import GHC.Base (divInt)
import Parser (fileP)
import System.FilePath as FilePath
import qualified Text.Parsec as Parsec
import Types
import Prelude hiding (error, lookup)

-- | Evaluates the given expression, yielding a value.
eval :: DExpr -> Interpreter DExpr
eval expr = do
  -- TODO: Finally
  pushExpr expr
  value <- eval'
  popExpr
  pure value

-- | Internal eval', using the getCurExpr.
eval' :: Interpreter DExpr
eval' = do
  expr <- getCurExpr
  case expr of
    -- Primitive values evaluate to themselves
    e@(DInt _) -> pure e
    e@(DString _) -> pure e
    e@(DChar _) -> pure e
    -- Empty lists evaluate to themselves
    e@(DList []) -> pure e
    -- Functions evaluate to themselves
    e@DFunction {} -> pure e
    -- Names dereference in the environment
    (DSymbol name) -> lookup name
    -- Named function calls
    DList (DSymbol name : params) ->
      special name params
    -- Unnamed function calls
    DList (p1 : params) -> general p1 params
  where
    -- special forms (like "if" and "fn") that require unique evaluation
    special :: String -> [DExpr] -> Interpreter DExpr
    -- def! binds the name to the given value
    special "def!" params = mdo
      (p1, p2) <- expect2 params
      name <- expectSymbol p1
      -- Note that the bound name is available in the environment when we evaluate the value
      boundEnv <- do
        bind name value
        getEnv
      value <- do
        putEnv boundEnv
        eval p2
      pure value

    -- let creates a new environment binds provided names to values in the new environment,
    -- evaluates the final expression in the context of the environment, and discards the env.
    -- special :: String -> [DExpr] -> Interpreter DExpr
    special "let" params = mdo
      (bindings', finalExpr) <- expect2 params
      bindings <- expectList bindings' >>= expectPairs
      -- A lazy binding allows us to create a binding while deferring the evaluation of the value
      -- until the binding is needed. This allows us to create recursive values.
      let createLazyBinding :: (String, DExpr) -> Interpreter ()
          createLazyBinding (name, value) =
            let lazyBinding = do
                  -- When the lazy binding is eventually evaluated, we want to reinstantiate
                  -- the "boundEnv" below.
                  putEnv boundEnv
                  eval value
             in bindLazy name lazyBinding
      pushEnv
      createLazyBinding `mapM_` bindings
      boundEnv <- getEnv
      -- We lookup all the bindings from left to right, effectively un-lazying them.
      lookup `mapM_` (fst <$> bindings)
      finalValue <- eval finalExpr
      popEnv
      pure finalValue

    -- Function definition
    special "fn" params = do
      (paramNames, result) <- expectFnDef params
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

    -- quote
    special "quote" params = expect1 params
    -- quasiquote
    special "quasiquote" params =
      let unquote :: DExpr -> Interpreter [DExpr]
          unquote (DList (DSymbol "unquote" : params')) = do
            p1' <- expect1 params' >>= eval
            pure [p1']
          unquote (DList (DSymbol "unquote-splice" : params')) = do
            expect1 params' >>= eval >>= expectList
          -- Recursively search list, unquoting children
          unquote (DList params') = do
            flattened <- concat <$> (unquote `mapM` params')
            pure [DList flattened]
          unquote expr = pure [expr]
       in expect1 params >>= unquote >>= expect1
    -- General function calls
    special first params = general (DSymbol first) params

    -- General function calls
    general first params = do
      first' <- eval first
      case first' of
        DFunction fn -> do
          evalParams <- eval `mapM` params
          fn evalParams
        e ->
          iError $ TypeError TFunction $ typeOf e

-- | Recursively looks up a value in the environment stack, potentially evaluating lazy bindings.
lookup :: String -> Interpreter DExpr
lookup name = do
  let lookup' :: [Env] -> Interpreter DExpr
      lookup' [] = iError $ ReferenceError name
      lookup' (head : tail) =
        case Map.lookup name head of
          Nothing -> lookup' tail
          Just (StrictBinding expr) -> pure expr
          Just (LazyBinding ref) -> do
            ref' <- readIORef ref
            case ref' of
              -- Lazy expression has already been evaluated; return it.
              Evaluated expr -> pure expr
              -- Deferred expression has yet to be evaluated. Evaluate it and rebind accordingly.
              Deferred deferred -> do
                expr <- deferred
                writeIORef ref $ Evaluated expr
                pure expr
  env <- getEnv
  lookup' env

-- | A closure is created whenever a function is defined. It consists of three things:
-- | 1. The current environment that the function body can refer to.
-- | 2. A list of parameters.
-- | 3. The function body - an expression to be evaluated when the function is called.
makeClosure :: [String] -> DExpr -> Interpreter DExpr
makeClosure names value = do
  closedEnv <- getEnv
  pure $
    DFunction
      ( \params -> do
          -- Temporarily save our existing environment so we can replace it after the function call
          tempEnv <- getEnv
          putEnv closedEnv
          pushEnv
          bindAll names params
          res <- eval value
          putEnv tempEnv
          pure res
      )

-- | Bindings for builtin functions and values.
builtins :: Env
builtins =
  StrictBinding
    <$> Map.fromList
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
        ),
        ( "<",
          DFunction $ \params -> do
            (p1, p2) <- expectInt `mapM` params >>= expect2
            pure $ toDBool $ p1 < p2
        ),
        ( ">",
          DFunction $ \params -> do
            (p1, p2) <- expectInt `mapM` params >>= expect2
            pure $ toDBool $ p1 > p2
        ),
        ( "typeof",
          DFunction $ \params -> do
            p1 <- expect1 params
            pure $ DSymbol $ show $ typeOf p1
        ),
        ("list", DFunction $ \params -> pure $ DList params),
        ( "count",
          DFunction $ \params -> do
            p1 <- expect1 params
            case p1 of
              DList l -> pure $ DInt $ length l
              DString s -> pure $ DInt $ length s
              e -> iError $ TypeError TList $ typeOf e
        ),
        ( "head",
          DFunction $ \params -> do
            p1 <- expect1 params
            case p1 of
              DList [] -> iError EmptyListError
              DList l -> pure $ head l
              DString "" -> iError EmptyListError
              DString s -> pure $ DChar $ head s
              e -> iError $ TypeError TList $ typeOf e
        ),
        ( "tail",
          DFunction $ \params -> do
            p1 <- expect1 params
            case p1 of
              DList [] -> iError EmptyListError
              DList l -> pure $ DList $ tail l
              DString "" -> iError EmptyListError
              DString s -> pure $ DString $ tail s
              e -> iError $ TypeError TList $ typeOf e
        ),
        ( "cons",
          DFunction $ \params -> do
            (h, t) <- expect2 params
            case t of
              DList l -> pure $ DList $ h : l
              DString s -> (\c -> DString $ c : s) <$> expectChar h
              e -> iError $ TypeError TList $ typeOf e
        ),
        ( "show",
          DFunction $ \params -> do
            p1 <- expect1 params
            pure $ DString $ show p1
        ),
        ( "error",
          DFunction $ \params -> do
            p1 <- expect1 params >>= expectString
            iError $ RuntimeError p1
        ),
        ( "print",
          DFunction $ \params -> do
            p1 <- expect1 params
            liftIO $ print p1
            pure p1
        ),
        ( "read-file",
          DFunction $ \params -> do
            p1 <- expect1 params >>= expectString
            fileName <- relativePath p1
            content <- liftIO $ readFile fileName
            pure $ DString content
        ),
        -- Runs the decent expressions in the given file
        ( "load-file",
          DFunction $ \params -> do
            p1 <- expect1 params >>= expectString
            runFile p1
        ),
        ( "concat",
          DFunction $ \params -> do
            strings <- expectString `mapM` params
            pure $ DString $ foldl (<>) "" strings
        ),
        ( "string-to-int",
          DFunction $ \params -> do
            p1 <- expect1 params >>= expectString
            pure $ DInt $ read p1
        )
      ]

-- | The initial state with builtins bound.
initState :: IO IState
initState =
  pure $ IState {envStack = [builtins], exprStack = [], importDir = ""}

-- | Interpreters the given path relative to the import dir.
-- TODO: The file name of the current file should be stored in a variable - not in state.
relativePath :: FilePath -> Interpreter FilePath
relativePath p = (</> p) <$> getImportDir

runFile :: String -> Interpreter DExpr
runFile fileName = do
  tryingFile <- relativePath fileName
  content <- liftIO $ readFile tryingFile
  let newDir = FilePath.takeDirectory fileName
  prevDir <- getImportDir
  setImportDir newDir
  case Parsec.parse fileP fileName (Text.pack content) of
    Left err -> do
      -- TODO: Issue with finaly
      setImportDir prevDir
      iError $ SyntaxError $ show err
    Right exprs -> do
      eval `mapM` exprs
      -- TODO: Issue with finaly
      setImportDir prevDir
      pure dNil

-- | Runs the interpreter with the provided state, yielding the result and the next state.
execInterpreter :: Interpreter a -> IState -> IO (Either IError a, IState)
-- execInterpreter interp state = runExceptT $ runStateT interp state
execInterpreter interp = runStateT (runExceptT interp)

-- | Runs the interpreter with the initial state, discarding the final state.
evalInterpreter :: Interpreter a -> IO (Either IError a)
evalInterpreter interpreter = do
  initState' <- initState
  fst <$> execInterpreter interpreter initState'
