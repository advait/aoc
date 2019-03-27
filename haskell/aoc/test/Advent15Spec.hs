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
  describe "world1" $ do
    let l1 = "####"
    let l2 = "#GE#"
    let l3 = "####"
    let world1 = readWorld $ intercalate "\n" [l1, l2, l3]
    it "Parses properly" $ do
      getPiece (WorldPos world1 $ Pos 1 1) `shouldBe` (Just $ Humanoid Goblin 200)
      getPiece (WorldPos world1 $ Pos 2 1) `shouldBe` (Just $ Humanoid Elf 200)
    it "Performs an attack on the first play" $ do
      let world1' = play world1 $ Pos 1 1
      getPiece (WorldPos world1' $ Pos 1 1) `shouldBe` (Just $ Humanoid Goblin 200)
      getPiece (WorldPos world1' $ Pos 2 1) `shouldBe` (Just $ Humanoid Elf 197)
    it "Performs two attacks after the first whole round" $ do
      let world1' = playRound world1
      getPiece (WorldPos world1' $ Pos 1 1) `shouldBe` (Just $ Humanoid Goblin 197)
      getPiece (WorldPos world1' $ Pos 2 1) `shouldBe` (Just $ Humanoid Elf 197)
    it "Keeps attacking until the elf is dead" $ do
      let (_, world1') = playAllRounds 0 world1
      getPiece (WorldPos world1' $ Pos 1 1) `shouldBe` (Just $ Humanoid Goblin 2)
      getPiece (WorldPos world1' $ Pos 2 1) `shouldBe` Nothing
  describe "world2" $ do
    let l1 = "#####"
    let l2 = "#G..#"
    let l3 = "#..E#"
    let l4 = "#####"
    let world2 = readWorld $ intercalate "\n" [l1, l2, l3, l4]
    it "Parses properly" $ do
      getPiece (WorldPos world2 $ Pos 1 1) `shouldBe` (Just $ Humanoid Goblin 200)
      getPiece (WorldPos world2 $ Pos 3 2) `shouldBe` (Just $ Humanoid Elf 200)
    it "Moves on the first play" $ do
      let world2' = play world2 $ Pos 1 1
      getPiece (WorldPos world2' $ Pos 1 1) `shouldBe` Nothing
      getPiece (WorldPos world2' $ Pos 2 1) `shouldBe` (Just $ Humanoid Goblin 200)
      getPiece (WorldPos world2' $ Pos 3 2) `shouldBe` (Just $ Humanoid Elf 200)
    it "Both pieces move on the first round" $ do
      let world2' = playRound world2
      getPiece (WorldPos world2' $ Pos 1 1) `shouldBe` Nothing
      getPiece (WorldPos world2' $ Pos 2 1) `shouldBe` (Just $ Humanoid Goblin 197)
      getPiece (WorldPos world2' $ Pos 3 2) `shouldBe` Nothing
      getPiece (WorldPos world2' $ Pos 3 1) `shouldBe` (Just $ Humanoid Elf 200)
    it "Keeps attacking until the elf is dead" $ do
      let (_, world2') = playAllRounds 0 world2
      getPiece (WorldPos world2' $ Pos 2 1) `shouldBe` Nothing
      getPiece (WorldPos world2' $ Pos 3 1) `shouldBe` (Just $ Humanoid Elf 2)
  describe "world3" $ do
    let l1 = "#######"
    let l2 = "#.G...#"
    let l3 = "#...EG#"
    let l4 = "#.#.#G#"
    let l5 = "#..G#E#"
    let l6 = "#.....#"
    let l7 = "#######"
    let world3 = readWorld $ intercalate "\n" [l1, l2, l3, l4, l5, l6, l7]
    it "Parses properly" $ do
      getPiece (WorldPos world3 $ Pos 2 1) `shouldBe` (Just $ Humanoid Goblin 200)
      getPiece (WorldPos world3 $ Pos 4 2) `shouldBe` (Just $ Humanoid Elf 200)
    it "Looks right after one full round" $ do
      let world3' = playRound world3
      getPiece (WorldPos world3' $ Pos 3 1) `shouldBe` (Just $ Humanoid Goblin 200)
      getPiece (WorldPos world3' $ Pos 4 2) `shouldBe` (Just $ Humanoid Elf 197)
      getPiece (WorldPos world3' $ Pos 5 2) `shouldBe` (Just $ Humanoid Goblin 197)
      getPiece (WorldPos world3' $ Pos 3 3) `shouldBe` (Just $ Humanoid Goblin 200)
      getPiece (WorldPos world3' $ Pos 5 3) `shouldBe` (Just $ Humanoid Goblin 197)
      getPiece (WorldPos world3' $ Pos 5 4) `shouldBe` (Just $ Humanoid Elf 197)
    it "Looks right after two full rounds" $ do
      let world3'' = foldl (\world _ -> playRound world) world3 [1 .. 2]
      getPiece (WorldPos world3'' $ Pos 4 1) `shouldBe` (Just $ Humanoid Goblin 200)
      getPiece (WorldPos world3'' $ Pos 3 2) `shouldBe` (Just $ Humanoid Goblin 200)
      getPiece (WorldPos world3'' $ Pos 4 2) `shouldBe` (Just $ Humanoid Elf 188)
      getPiece (WorldPos world3'' $ Pos 5 2) `shouldBe` (Just $ Humanoid Goblin 194)
      getPiece (WorldPos world3'' $ Pos 5 3) `shouldBe` (Just $ Humanoid Goblin 194)
      getPiece (WorldPos world3'' $ Pos 5 4) `shouldBe` (Just $ Humanoid Elf 194)
    it "Looks right after 23 full rounds when first elf dies" $ do
      let world3'23 = foldl (\world _ -> playRound world) world3 [1 .. 23]
      getPiece (WorldPos world3'23 $ Pos 4 1) `shouldBe` (Just $ Humanoid Goblin 200)
      getPiece (WorldPos world3'23 $ Pos 3 2) `shouldBe` (Just $ Humanoid Goblin 200)
      getPiece (WorldPos world3'23 $ Pos 4 2) `shouldBe` Nothing
      getPiece (WorldPos world3'23 $ Pos 5 3) `shouldBe` (Just $ Humanoid Goblin 131)
      getPiece (WorldPos world3'23 $ Pos 5 4) `shouldBe` (Just $ Humanoid Elf 131)
    it "Looks right after all rounds have been played" $ do
      let (round, world3') = playAllRounds 0 world3
      round `shouldBe` 47
      getPiece (WorldPos world3' $ Pos 1 1) `shouldBe` (Just $ Humanoid Goblin 200)
      getPiece (WorldPos world3' $ Pos 2 2) `shouldBe` (Just $ Humanoid Goblin 131)
      getPiece (WorldPos world3' $ Pos 5 3) `shouldBe` (Just $ Humanoid Goblin 59)
      getPiece (WorldPos world3' $ Pos 5 5) `shouldBe` (Just $ Humanoid Goblin 200)



    {- #.G...#   G(200)
       #...EG#   E(200), G(200)
       #.#.#G#   G(200)
       #..G#E#   G(200), E(200)
       #.....#
       #######
       -}
  {-
  let world3 = readWorld $ intercalate "\n" [m1, m2, m3, m4]
  let wpcToPos (WorldPos _ p, _) = p
  describe "readWorld" $
    it "Parses world1 properly" $ do
      (getPiece . WorldPos world1 $ Pos 0 0) `shouldBe` Just Wall
      (getPiece . WorldPos world1 $ Pos 1 0) `shouldBe` Just Wall
      (getPiece . WorldPos world1 $ Pos 2 0) `shouldBe` Just Wall
      (getPiece . WorldPos world1 $ Pos 3 0) `shouldBe` Just Wall
      (getPiece . WorldPos world1 $ Pos 0 1) `shouldBe` Just Wall
      (getPiece . WorldPos world1 $ Pos 1 1) `shouldBe` Just (Humanoid Goblin 200)
      (getPiece . WorldPos world1 $ Pos 2 1) `shouldBe` Just (Humanoid Elf 200)
  describe "neighbors" $
    it "Detects free squares properly" $
    map wpcToPos (neighbors (WorldPos world2 (Pos 0 0), Goblin)) `shouldBe` [Pos 0 (-1), Pos (-1) 0, Pos 1 0, Pos 0 1]
  describe "shortestPath" $ do
    it "Detects the shortest path for a small map" $ do
      let goblin = (WorldPos world1 $ Pos 1 1, Goblin) :: WorldPosContext
      map wpcToPos <$> shortestPathBool goblin (isEnemy goblin) `shouldBe` Just [Pos 1 1, Pos 2 1]
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
 -}

instance Node Pos where
  neighbors (Pos x y) = sort [Pos (x - 1) y, Pos (x + 1) y, Pos x (y - 1), Pos x (y + 1)]
