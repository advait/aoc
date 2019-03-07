module Advent15 where

-- Represents an abstract node for pathfinding algorithms
class Ord a => Node a where
  neighbors :: a -> [a]

-- (X, Y) cartesian coordinates
data Pos = Pos Int Int
  deriving (Eq)

-- Order positions according to "reading order": y first then x.
instance Ord Pos where
  compare (Pos x1 y1) (Pos x2 y2) = compare (y1, x1) (y2, x2)

instance Show Pos where
  show (Pos x y) = show (x, y)

main :: IO ()
main = undefined