module Pathfinding where

import qualified Data.Map    as Map
import qualified Data.Set    as Set
import qualified Debug.Trace as Trace

-- Represents an abstract node for pathfinding algorithms
class (Ord a, Show a) =>
      Node a
  where
  neighbors :: a -> [a] -- Returns neighbors in *sorted* order

-- Returns the shortest path from the first node to the second or Nothing
-- if we have exhaustively determined that no such path exists.
shortestPath :: (Node a) => a -> a -> Maybe [a]
shortestPath src dst = shortestPathBool src (\n -> dst == n)

shortestPathBool :: (Node a) => a -> (a -> Bool) -> Maybe [a]
shortestPathBool src dstPred = processQueue startSeen startQueue
  where
    startSeen = Set.fromList [src]
    startQueue = [[src]]
    processQueue _ [] = Nothing
    processQueue seen (curPath:remainingPaths)
      | dstPred curNode = Just curPath
      | otherwise = processQueue newSeen newQueue
      where
        curNode = last curPath
        elligibleNeighbors = filter (`Set.notMember` seen) $ neighbors curNode
        newPaths = map (\n -> curPath ++ [n]) elligibleNeighbors
        newQueue = remainingPaths ++ newPaths
        newSeen = Set.union seen $ Set.fromList elligibleNeighbors
