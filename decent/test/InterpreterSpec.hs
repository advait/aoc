module InterpreterSpec where

import Control.Monad (unless)
import qualified Data.Text as Text
import Interpreter (eval, evalInterpreter)
import Parser (exprP)
import Test.Hspec (describe, it)
import Test.QuickCheck ()
import TestUtils (throwEither, throwInterpreter, throwParser)
import Types (Interpreter)

spec = do
  describe "Interpreter" $ do
    it "evaluate numbers" $ do
      "1" `shouldEvalTo` "1"
    it "evaluates math builtins" $ do
      "(+ 1 1)" `shouldEvalTo` "2"
      "(* (+ 1 3) (- 3 7))" `shouldEvalTo` "-16"
    it "handles scopes with let" $ do
      "(let (a 1) a)" `shouldEvalTo` "1"
      "(let (a 1 b 2) b)" `shouldEvalTo` "2"
      "(let (a 1 b 2) (+ a b))" `shouldEvalTo` "3"
      "(let (a 1) (+ (let (a 2) a) a))" `shouldEvalTo` "3"
    it "handles functions" $ do
      "((fn (a) (* a 2)) 2)" `shouldEvalTo` "4"
      "((fn (a b) (+ a b)) 2 3)" `shouldEvalTo` "5"
      "(let (double (fn (a) (* a 2))) (double 1))" `shouldEvalTo` "2"
      "(let (double (fn (a) (* a 2))) (double (double 2))))" `shouldEvalTo` "8"
    it "handles quoting" $ do
      "(let (a '(3 4)) `(1 2 ,a))" `shouldEvalTo` "(1 2 (3 4))"
      "(let (a '(3 4)) `(1 2 ,@a))" `shouldEvalTo` "(1 2 3 4)"
    it "handles varargs" $ do
      "((fn (n & a) `(,n ,a)) 1 2 3)" `shouldEvalTo` "(1 (2 3))"
      "((fn (& a) a) 1 2 3)" `shouldEvalTo` "(1 2 3)"
    it "handles macros" $ do
      "(do (defmacro m (a) a) \n (m 1))" `shouldEvalTo` "1"
      "(do (defmacro m (a) `(+ ,a 1)) \n (m 2))" `shouldEvalTo` "3"
    it "loads base" $ do
      "(load-file \"./src/Base.dc\")" `shouldEvalTo` "()"

shouldEvalTo :: String -> String -> IO ()
shouldEvalTo input expected' = do
  expr <- throwParser exprP input
  expected <- throwParser exprP expected'
  res <- throwInterpreter $ eval expr
  unless (res == expected) $ fail (show res <> " ≠ " <> expected')
