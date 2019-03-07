module Advent15 where

-- Represents an abstract node for pathfinding algorithms
class Ord a => Node a where
  -- Returns neighbors in *sorted* order
  neighbors :: a -> [a]

-- (X, Y) cartesian coordinates
data Pos = Pos Int Int
  deriving (Eq)

-- Order positions according to "reading order": y first then x.
instance Ord Pos where
  compare (Pos x1 y1) (Pos x2 y2) = compare (y1, x1) (y2, x2)

instance Show Pos where
  show (Pos x y) = show (x, y)

-- Alternative compare function for [Node]/Paths that prefers shorter paths.
comparePath :: (Node a) => [a] -> [a] -> Ordering
comparePath [] [] = EQ
comparePath a b
  | lenA < lenB = LT
  | lenA > lenB = GT
  | otherwise = compare a b
  where lenA = length a
        lenB = length b

main :: IO ()
main = undefined