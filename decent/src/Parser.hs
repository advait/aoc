module Parser where

import Control.Applicative ((<|>))
import Data.Char (isAlpha, isAlphaNum)
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Parsec (ParseError, ParsecT, Stream, char, choice, many, many1, oneOf, runParser, satisfy, sepBy, string, try)
import Types

type Parser a = ParsecT Text () Identity a

whitespace :: Parser ()
whitespace = () <$ many (oneOf [' ', '\t', '\n'])

-- | Wraps p so that it consumes trailing whitespace.
lexeme :: Parser a -> Parser a
lexeme p = do
  a <- p
  _ <- whitespace
  pure a

integerP :: Parser DExpr
integerP = do
  sign <- string "-" <|> string ""
  numbers <- many1 $ oneOf "0123456789"
  pure $ DInt $ read $ sign <> numbers

symbolP :: Parser DExpr
symbolP =
  let puncP = oneOf "!$',_-./:;?+<=>#%&*@[\\]{|}`^~"
   in do
        head <- satisfy isAlpha <|> puncP
        tail <- many (satisfy isAlphaNum <|> puncP)
        pure $ DSymbol $ head : tail

listP :: Parser DExpr
listP = do
  _ <- lexeme $ char '('
  items <- lexeme $ sepBy exprP whitespace
  _ <- char ')'
  pure $ DList items

parse :: String -> Either ParseError DExpr
parse input = runParser exprP () "(source)" (Text.pack input)

exprP =
  choice
    [ try integerP, -- "try" because "-" is an integer prefix as well as a symbol
      symbolP,
      listP
    ]