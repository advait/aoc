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
  describe "world1" $ do
    let l1 = "####"
    let l2 = "#GE#"
    let l3 = "####"
    let world1 = readWorld $ intercalate "\n" [l1, l2, l3]
    it "Parses properly" $ do
      getPiece (Pos 1 1, world1) `shouldBe` (Just $ Humanoid Goblin 200)
      getPiece (Pos 2 1, world1) `shouldBe` (Just $ Humanoid Elf 200)
    it "Performs an attack on the first play" $ do
      let world1' = play world1 $ Pos 1 1
      getPiece (Pos 1 1, world1') `shouldBe` (Just $ Humanoid Goblin 200)
      getPiece (Pos 2 1, world1') `shouldBe` (Just $ Humanoid Elf 197)
    it "Performs two attacks after the first whole round" $ do
      let world1' = playRound world1
      getPiece (Pos 1 1, world1') `shouldBe` (Just $ Humanoid Goblin 197)
      getPiece (Pos 2 1, world1') `shouldBe` (Just $ Humanoid Elf 197)
    it "Keeps attacking until the elf is dead" $ do
      let (_, world1') = playAllRounds 0 world1
      getPiece (Pos 1 1, world1') `shouldBe` (Just $ Humanoid Goblin 2)
      getPiece (Pos 2 1, world1') `shouldBe` Nothing
  describe "world2" $ do
    let l1 = "#####"
    let l2 = "#G..#"
    let l3 = "#..E#"
    let l4 = "#####"
    let world2 = readWorld $ intercalate "\n" [l1, l2, l3, l4]
    it "Parses properly" $ do
      getPiece (Pos 1 1, world2) `shouldBe` (Just $ Humanoid Goblin 200)
      getPiece (Pos 3 2, world2) `shouldBe` (Just $ Humanoid Elf 200)
    it "Moves on the first play" $ do
      let world2' = play world2 $ Pos 1 1
      getPiece (Pos 1 1, world2') `shouldBe` Nothing
      getPiece (Pos 2 1, world2') `shouldBe` (Just $ Humanoid Goblin 200)
      getPiece (Pos 3 2, world2') `shouldBe` (Just $ Humanoid Elf 200)
    it "Both pieces move on the first round, and the elf attacks" $ do
      let world2' = playRound world2
      getPiece (Pos 1 1, world2') `shouldBe` Nothing
      getPiece (Pos 2 1, world2') `shouldBe` (Just $ Humanoid Goblin 197)
      getPiece (Pos 3 2, world2') `shouldBe` Nothing
      getPiece (Pos 3 1, world2') `shouldBe` (Just $ Humanoid Elf 200)
    it "Keeps attacking until the goblin is dead" $ do
      let (_, world2') = playAllRounds 0 world2
      getPiece (Pos 3 1, world2') `shouldBe` (Just $ Humanoid Elf 2)
      getPiece (Pos 2 1, world2') `shouldBe` Nothing
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
      getPiece (Pos 2 1, world3) `shouldBe` (Just $ Humanoid Goblin 200)
      getPiece (Pos 4 2, world3) `shouldBe` (Just $ Humanoid Elf 200)
    it "Looks right after one full round" $ do
      let world3' = playRound world3
      getPiece (Pos 3 1, world3') `shouldBe` (Just $ Humanoid Goblin 200)
      getPiece (Pos 4 2, world3') `shouldBe` (Just $ Humanoid Elf 197)
      getPiece (Pos 5 2, world3') `shouldBe` (Just $ Humanoid Goblin 197)
      getPiece (Pos 3 3, world3') `shouldBe` (Just $ Humanoid Goblin 200)
      getPiece (Pos 5 3, world3') `shouldBe` (Just $ Humanoid Goblin 197)
      getPiece (Pos 5 4, world3') `shouldBe` (Just $ Humanoid Elf 197)
    it "Looks right after two full rounds" $ do
      let world3' = foldl (\world _ -> playRound world) world3 [1 .. 2]
      getPiece (Pos 4 1, world3') `shouldBe` (Just $ Humanoid Goblin 200)
      getPiece (Pos 3 2, world3') `shouldBe` (Just $ Humanoid Goblin 200)
      getPiece (Pos 4 2, world3') `shouldBe` (Just $ Humanoid Elf 188)
      getPiece (Pos 5 2, world3') `shouldBe` (Just $ Humanoid Goblin 194)
      getPiece (Pos 5 3, world3') `shouldBe` (Just $ Humanoid Goblin 194)
      getPiece (Pos 5 4, world3') `shouldBe` (Just $ Humanoid Elf 194)
    it "Looks right after 23 full rounds when first elf dies" $ do
      let world3' = foldl (\world _ -> playRound world) world3 [1 .. 23]
      getPiece (Pos 4 1, world3') `shouldBe` (Just $ Humanoid Goblin 200)
      getPiece (Pos 3 2, world3') `shouldBe` (Just $ Humanoid Goblin 200)
      getPiece (Pos 4 2, world3') `shouldBe` Nothing
      getPiece (Pos 5 3, world3') `shouldBe` (Just $ Humanoid Goblin 131)
      getPiece (Pos 5 4, world3') `shouldBe` (Just $ Humanoid Elf 131)
    it "Looks right after all rounds have been played" $ do
      let (round, world3') = playAllRounds 0 world3
      round `shouldBe` 46
      getPiece (Pos 1 1, world3') `shouldBe` (Just $ Humanoid Goblin 200)
      getPiece (Pos 2 2, world3') `shouldBe` (Just $ Humanoid Goblin 131)
      getPiece (Pos 5 3, world3') `shouldBe` (Just $ Humanoid Goblin 59)
      getPiece (Pos 5 5, world3') `shouldBe` (Just $ Humanoid Goblin 200)
  describe "summarizeCombat" $
    it "Works for example #1" $ do
      let l1 = "#######"
      let l2 = "#G..#E#"
      let l3 = "#E#E.E#"
      let l4 = "#G.##.#"
      let l5 = "#...#E#"
      let l6 = "#...E.#"
      let l7 = "#######"
      let example1 = readWorld $ intercalate "\n" [l1, l2, l3, l4, l5, l6, l7]
      let (round, example1') = playAllRounds 0 example1
      let cases = [(Pos 5 1, 200), (Pos 1 2, 197), (Pos 2 3, 185), (Pos 1 4, 200), (Pos 5 4, 200)]
      let assertions = map (\(p, h) -> getPiece (p, example1') `shouldBe` (Just $ Humanoid Elf h)) cases
      sequence_ assertions
      round `shouldBe` 37
      summarizeCombat example1 `shouldBe` 36334

instance Node Pos where
  neighbors (Pos x y) = sort [Pos (x - 1) y, Pos (x + 1) y, Pos x (y - 1), Pos x (y + 1)]
