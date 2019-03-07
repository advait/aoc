module Advent15Spec where

import           Advent15
import           Data.List
import qualified Data.Maybe  as Maybe
import qualified Data.Set    as Set
import qualified Debug.Trace as Trace
import           Test.Hspec

spec :: Spec
spec = do
  describe "Pos" $ do
    it "Compares Y before X" $ compare (Pos 1 0) (Pos 0 1) `shouldBe` LT
    it "Orders items on the same row" $ sort [Pos 1 0, Pos 0 0] `shouldBe` [Pos 0 0, Pos 1 0]
    it "Orders Y before X in sort" $
      sort [Pos 1 1, Pos 0 0, Pos 0 1, Pos 1 0] `shouldBe` [Pos 0 0, Pos 1 0, Pos 0 1, Pos 1 1]
    it "Selects adjacent neighbors in sorted order" $
      neighbors (Pos 0 0) `shouldBe` sort [Pos 0 1, Pos 0 (-1), Pos 1 0, Pos (-1) 0]
  describe "comparePath" $ do
    it "Prefers shorter paths to the same destination" $ do
      let p1 = [Pos 0 0, Pos 1 0, Pos 1 1]
      let p2 = [Pos 0 0, Pos 0 1, Pos 0 2, Pos 1 2, Pos 1 1]
      comparePath p1 p2 `shouldBe` LT
    it "Prefers three-length paths according to reading order" $ do
      let p1 = [Pos 0 0, Pos 1 0, Pos 1 1]
      let p2 = [Pos 0 0, Pos 0 1, Pos 1 1]
      comparePath p1 p2 `shouldBe` LT
  describe "Set Path" $
    it "Detects inserted elements properly" $ do
      let s1 = Set.insert (Pos 0 0) Set.empty
      Set.member (Pos 0 0) s1 `shouldBe` True
      Set.member (Pos 0 1) s1 `shouldBe` False
      let s2 = Set.fromList $ map (Pos 0) [0 .. 100]
      Set.member (Pos 0 69) s2 `shouldBe` True
  describe "shortestPath" $ do
    it "Returns a one-length path when the src == dst" $ shortestPath (Pos 0 0) (Pos 0 0) `shouldBe` Just [Pos 0 0]
    it "Returns a two-length path when dst is a neighbor of src" $ do
      shortestPath (Pos 0 0) (Pos 0 1) `shouldBe` Just [Pos 0 0, Pos 0 1]
      shortestPath (Pos 0 0) (Pos 1 0) `shouldBe` Just [Pos 0 0, Pos 1 0]
      shortestPath (Pos 0 0) (Pos 0 (-1)) `shouldBe` Just [Pos 0 0, Pos 0 (-1)]
      shortestPath (Pos 0 0) (Pos (-1) 0) `shouldBe` Just [Pos 0 0, Pos (-1) 0]
    it "Returns the reading-ordering three-length path appropriately" $ do
      shortestPath (Pos 0 0) (Pos 1 1) `shouldBe` Just [Pos 0 0, Pos 1 0, Pos 1 1]
      shortestPath (Pos 0 0) (Pos (-1) 1) `shouldBe` Just [Pos 0 0, Pos (-1) 0, Pos (-1) 1]
      shortestPath (Pos 0 0) (Pos 1 (-1)) `shouldBe` Just [Pos 0 0, Pos 0 (-1), Pos 1 (-1)]
      shortestPath (Pos 0 0) (Pos (-1) (-1)) `shouldBe` Just [Pos 0 0, Pos 0 (-1), Pos (-1) (-1)]
    it "Returns a path of length (n+1) if dst is n units away from the origin" $ do
      let n = 10
      let path = map (Pos 0) [0 .. n]
      shortestPath (Pos 0 0) (Pos 0 n) `shouldBe` Just path

-- As a test, allow Pos to implement the Node class
instance Node Pos where
  neighbors (Pos x y) = sort [Pos (x - 1) y, Pos (x + 1) y, Pos x (y - 1), Pos x (y + 1)]
