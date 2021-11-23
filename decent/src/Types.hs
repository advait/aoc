module Types where

-- | An expression that can be evaluated.
data DExpr
  = DSymbol String
  | DInt Int
  | DList [DExpr] -- A DList is a lisp S-Expression
  deriving (Eq)

instance Show DExpr where
  show (DSymbol symbol) = symbol
  show (DInt int) = show int
  show (DList l) = "(" <> unwords (show <$> l) <> ")"
