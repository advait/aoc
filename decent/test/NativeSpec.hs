module NativeSpec where

import Interpreter (runFile)
import Test.Hspec (describe, it)
import TestUtils (throwInterpreter)

spec = do
  describe "Native tests" $ do
    it "Runs all native tests successfully" $ do
      _ <- throwInterpreter $ runFile "./test/BaseTest.dc"
      pure ()
