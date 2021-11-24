module ParserSpec where

import Control.Monad (unless)
import qualified Data.Text as Text
import Parser
import Test.Hspec
import Test.QuickCheck
import Text.Parsec
import Types

spec = do
  describe "Parser" $ do
    it "parses integers" $
      property $ \x -> exprP `shouldParse` DInt (x :: Int)
    it "parses symbols" $ do
      exprP `shouldParse` DSymbol "hello"
    it "parses lists" $ do
      exprP `shouldParse` DList []
      exprP `shouldParse` DList [DInt 3]

shouldParse :: (Eq a, Show a) => Parser a -> a -> IO ()
shouldParse parser a =
  case runParser parser () "(source)" (Text.pack $ show a) of
    Left err -> fail $ show err
    Right res -> unless (a == res) $ fail (show a <> " ≠ " <> show res)
