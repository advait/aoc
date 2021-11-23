module ParserSpec where

import Control.Monad (unless)
import qualified Data.Text as Text
import Parser
import Test.Hspec
import Test.QuickCheck
import Text.Parsec
import Types

spec = do
  describe "parser" $ do
    it "parses integers" $
      property $ \x -> integer `shouldParse` DInt (x :: Int)

shouldParse :: (Eq a, Show a) => Parser a -> a -> IO ()
shouldParse parser a =
  case runParser parser () "(source)" (Text.pack $ show a) of
    Left err -> fail $ show err
    Right res -> unless (a == res) $ fail (show a <> " ≠ " <> show res)
