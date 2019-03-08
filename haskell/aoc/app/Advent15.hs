module Advent15 where

import qualified Data.Map    as Map
import qualified Data.Set    as Set
import qualified Debug.Trace as Trace
import           Pathfinding

-- (X, Y) cartesian coordinates
data Pos =
  Pos Int
      Int
  deriving (Eq)

-- Order positions according to "reading order": y first then x.
instance Ord Pos where
  compare (Pos x1 y1) (Pos x2 y2) = compare (y1, x1) (y2, x2)

instance Show Pos where
  show (Pos x y) = show (x, y)

main :: IO ()
main = undefined
