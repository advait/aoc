module Advent15Spec where

import           Advent15
import           Data.List
import qualified Data.Map    as Map
import qualified Data.Maybe  as Maybe
import qualified Data.Set    as Set
import qualified Debug.Trace as Trace
import           Pathfinding
import           Test.Hspec

spec :: Spec
spec = do
  describe "Pos" $ do
    it "Compares Y before X" $ compare (Pos 1 0) (Pos 0 1) `shouldBe` LT
    it "Orders items on the same row" $ sort [Pos 1 0, Pos 0 0] `shouldBe` [Pos 0 0, Pos 1 0]
    it "Orders Y before X in sort" $
      sort [Pos 1 1, Pos 0 0, Pos 0 1, Pos 1 0] `shouldBe` [Pos 0 0, Pos 1 0, Pos 0 1, Pos 1 1]
  describe "comparePath" $ do
    it "Prefers shorter paths to the same destination" $ do
      let p1 = [Pos 0 0, Pos 1 0, Pos 1 1]
      let p2 = [Pos 0 0, Pos 0 1, Pos 0 2, Pos 1 2, Pos 1 1]
      comparePath p1 p2 `shouldBe` LT
    it "Prefers three-length paths according to reading order" $ do
      let p1 = [Pos 0 0, Pos 1 0, Pos 1 1]
      let p2 = [Pos 0 0, Pos 0 1, Pos 1 1]
      comparePath p1 p2 `shouldBe` LT
  let l1 = "####"
  let l2 = "#GE#"
  let l3 = "####"
  let world1 = readWorld $ intercalate "\n" [l1, l2, l3]
  let world2 = readWorld "G"
  let m1 = "#####"
  let m2 = "#G..#"
  let m3 = "#..E#"
  let m4 = "#####"
  let world3 = readWorld $ intercalate "\n" [m1, m2, m3, m4]
  let wpToPos (WorldPos _ p) = p
  describe "readWorld" $
    it "Parses world1 properly" $ do
      (getPiece . WorldPos world1 $ Pos 0 0) `shouldBe` Just Wall
      (getPiece . WorldPos world1 $ Pos 1 0) `shouldBe` Just Wall
      (getPiece . WorldPos world1 $ Pos 2 0) `shouldBe` Just Wall
      (getPiece . WorldPos world1 $ Pos 3 0) `shouldBe` Just Wall
      (getPiece . WorldPos world1 $ Pos 0 1) `shouldBe` Just Wall
      (getPiece . WorldPos world1 $ Pos 1 1) `shouldBe` Just (Goblin 200)
      (getPiece . WorldPos world1 $ Pos 2 1) `shouldBe` Just (Elf 200)
  describe "neighbors" $ do
    it "Detects free squares properly" $
      (map wpToPos . (neighbors . WorldPos world2 $ Pos 0 0) . WorldPos world2 $ Pos 0 0) `shouldBe`
      [Pos 0 (-1), Pos (-1) 0, Pos 1 0, Pos 0 1]
    it "Interprets enemy-occupied squares as navigable" $
      (map wpToPos . (neighbors . WorldPos world1 $ Pos 1 1) `shouldBe` [Pos 2 1]
  describe "shortestPath" $ do
    it "Detects the shortest path for a small map" $ do
      let goblin = WorldPos world1 $ Pos 1 1
      map wpToPos <$> shortestPathBool goblin (isEnemy goblin) `shouldBe` Just [Pos 1 1, Pos 2 1]
      1 `shouldBe` 1
    it "Detects paths in reading order" $ do
      let wpOf = WorldPos world3
      (map wpToPos . neighbors (wpOf $ Pos 1 1) . wpOf $ Pos 1 1) `shouldBe` [Pos 2 1, Pos 1 2]
      (map wpToPos . neighbors (wpOf $ Pos 2 1) . wpOf $ Pos 2 1) `shouldBe` [Pos 3 1, Pos 2 2]
      map wpToPos <$> shortestPath (wpOf $ Pos 1 1) (wpOf $ Pos 1 1) `shouldBe` Just [Pos 1 1]
      map wpToPos <$> shortestPath (wpOf $ Pos 1 1) (wpOf $ Pos 1 2) `shouldBe` Just [Pos 1 1, Pos 1 2]
      map wpToPos <$> shortestPath (wpOf $ Pos 1 1) (wpOf $ Pos 2 1) `shouldBe` Just [Pos 1 1, Pos 2 1]
      map wpToPos <$> shortestPath (wpOf $ Pos 1 1) (wpOf $ Pos 3 1) `shouldBe` Just [Pos 1 1, Pos 2 1, Pos 3 1]
      map wpToPos <$>
        shortestPath (wpOf $ Pos 1 1) (wpOf $ Pos 3 2) `shouldBe` Just [Pos 1 1, Pos 2 1, Pos 3 1, Pos 3 2]
    it "Detects paths from goblins to elves" $ do
      let goblin = WorldPos world3 $ Pos 1 1
      map wpToPos <$> shortestPathBool goblin (isEnemy goblin) `shouldBe` Just [Pos 1 1, Pos 2 1, Pos 3 1, Pos 3 2]

-- As a test, allow Pos to implement the Node class
instance Node Pos where
  neighbors src (Pos x y) = sort [Pos (x - 1) y, Pos (x + 1) y, Pos x (y - 1), Pos x (y + 1)]
