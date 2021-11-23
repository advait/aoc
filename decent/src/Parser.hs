module Parser where

import Control.Applicative ((<|>))
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import Text.Parsec (ParsecT, Stream, char, choice, many, many1, oneOf, sepBy, string)
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

listP :: Parser DExpr
listP = do
  _ <- lexeme $ char '('
  items <- lexeme $ sepBy exprP whitespace
  _ <- lexeme $ char ')'
  pure $ DList items

exprP = choice [integerP, listP]
