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

shouldParse :: DExpr -> IO ()
shouldParse a = do
  case runParser exprP () "" (Text.pack $ show a) of
    Left err -> fail $ show err
    Right res -> unless (a == res) $ fail (show a <> " ≠ " <> show res)
