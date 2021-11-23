module Parser where

import Control.Applicative ((<|>))
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import Text.Parsec (ParsecT, Stream, many1, oneOf, string)
import Types

type Parser a = ParsecT Text () Identity a

whitespace :: Parser ()
whitespace = () <$ oneOf [' ', '\t', '\n']

integer :: Parser DExpr
integer = do
  sign <- string "-" <|> string ""
  numbers <- many1 (oneOf "0123456789")
  pure $ DInt $ read $ sign <> numbers

-- | Wraps p so that it consumes trailing whitespace.
lexeme :: Parser a -> Parser a
lexeme p = do
  a <- p
  _ <- whitespace
  pure a
