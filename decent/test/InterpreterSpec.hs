module InterpreterSpec where

import Control.Monad (unless)
import qualified Data.Text as Text
import Interpreter
import Parser
import Test.Hspec
import Test.QuickCheck
import TestUtils
import Text.Parsec
import Types

spec = do
  describe "Interpreter" $ do
    it "evaluate numbers" $ do
      "1" `shouldEvalTo` "1"
    it "evaluates math builtins" $ do
      "(+ 1 1)" `shouldEvalTo` "2"
      "(* (+ 1 3) (- 3 7))" `shouldEvalTo` "-16"
    it "handles scopes with let" $ do
      "(let (a 1) a)" `shouldEvalTo` "1"
      "(let (a 1 b 2) (+ a b))" `shouldEvalTo` "3"
      "(let (a 1) (+ (let (a 2) a) a))" `shouldEvalTo` "3"

shouldEvalTo :: String -> String -> IO ()
shouldEvalTo input expected' = do
  expr <- throwParser exprP input
  expected <- throwParser exprP expected'
  res <- throwInterpreter $ eval expr
  unless (res == expected) $ fail (show res <> " ≠ " <> expected')

-- | Runs the interpreter, throwing if we get an error.
throwInterpreter :: Interpreter a -> IO a
throwInterpreter = throwEither . evalInterpreter
