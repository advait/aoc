module Parser where

import Control.Applicative (optional, (<|>))
import Data.Char (isAlpha, isAlphaNum, isPrint)
import Data.Functor.Identity (Identity)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Parsec (ParseError, ParsecT, Stream, char, choice, many, many1, oneOf, runParser, satisfy, sepBy, string, try)
import qualified Text.Parsec as Parsec
import Text.Parsec.Combinator (eof)
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

stringP :: Parser DExpr
stringP = do
  _ <- char '"'
  body <- many (escaped <|> unescaped)
  _ <- char '"'
  pure $ DString body
  where
    escaped :: Parser Char
    escaped = do
      _ <- char '\\'
      Parsec.choice
        [ '\n' <$ char 'n',
          '\t' <$ char 't',
          '"' <$ char '"',
          '\\' <$ char '\\'
        ]

    unescaped :: Parser Char
    unescaped = satisfy $ \c -> '\\' /= c && '"' /= c && isPrint c

symbolP :: Parser DExpr
symbolP =
  let puncP = oneOf "!$',_-./:?+<=>#%&*@[\\]{|}`^~"
   in do
        head <- satisfy isAlpha <|> puncP
        tail <- many (satisfy isAlphaNum <|> puncP)
        pure $ DSymbol $ head : tail

listP :: Parser DExpr
listP = do
  _ <- lexeme $ char '('
  items <- many $ lexeme exprP
  _ <- lexeme $ char ')'
  pure $ DList items

-- | Shorthand notation for quoted expressions
quotedP :: Parser DExpr
quotedP =
  let quote = do
        _ <- char '\''
        (\expr -> DList [DSymbol "quote", expr]) <$> exprP

      quasi = do
        _ <- char '`'
        (\expr -> DList [DSymbol "quasiquote", expr]) <$> exprP

      unquote = do
        _ <- char ','
        (\expr -> DList [DSymbol "unquote", expr]) <$> exprP
   in choice [quote, quasi, unquote]

-- | Main expression parser
exprP :: Parser DExpr
exprP =
  choice
    [ quotedP,
      stringP,
      try integerP, -- "try" because "-" is an integer prefix as well as a symbol
      symbolP,
      listP
    ]

-- | Comment parser
commentP :: Parser ()
commentP = do
  _ <- char ';'
  _ <- many $ satisfy $ (/=) '\n'
  (() <$ char '\n') <|> eof

-- | Parses a line from the repl, ignoring comments.
replLineP :: Parser (Maybe DExpr)
replLineP = do
  whitespace
  expr <- optional $ lexeme exprP
  optional commentP
  eof
  pure expr

-- | Parses a file, stripping comments and returning the list of expressions.
fileP :: Parser [DExpr]
fileP = do
  whitespace
  exprs <- catMaybes <$> many (exprP' <|> commentP')
  eof
  pure exprs
  where
    exprP' = lexeme (Just <$> exprP)
    commentP' = lexeme $ Nothing <$ commentP

parse :: String -> Either ParseError DExpr
parse input = Parsec.parse exprP "" (Text.pack input)
