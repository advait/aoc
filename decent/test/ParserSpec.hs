module ParserSpec where

import Control.Monad (unless)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import qualified Data.Text as Text
import Parser (Parser, exprP)
import Test.Hspec (describe, it)
import Test.QuickCheck (Testable (property))
import Text.Parsec (runParser)
import Types (DExpr (DInt, DList, DString, DSymbol))

spec = do
  describe "Parser" $ do
    it "parses integers" $
      property $ \x -> shouldParse $ DInt (x :: Int)
    it "parses strings" $
      let acceptableChars = filter $ \c -> elem c ['\n', '\t', '"', '\\'] || isAsciiUpper c || isAsciiLower c || isDigit c
       in property $ \x -> shouldParse $ DString $ acceptableChars (x :: String)
    it "parses symbols" $ do
      shouldParse $ DSymbol "hello"
    it "parses lists" $ do
      shouldParse $ DList []
      shouldParse $ DList [DInt 3]
    it "parses quotes" $ do
      "'a" `shouldParseTo` DList [DSymbol "quote", DSymbol "a"]

shouldParse :: DExpr -> IO ()
shouldParse a = show a `shouldParseTo` a

shouldParseTo :: String -> DExpr -> IO ()
shouldParseTo from to = do
  case runParser exprP () "" (Text.pack from) of
    Left err -> fail $ show err
    Right res -> unless (to == res) $ fail (show to <> " ≠ " <> show res)
