module Advent15Spec where

import           Advent15
import           Data.Foldable
import qualified Data.IntMap   as IntMap
import           Data.List
import qualified Data.Maybe    as Maybe
import qualified Debug.Trace   as Trace
import qualified Deque
import           Test.Hspec

instance Node Pos where
  neighbors (Pos x y) = sort [Pos (x - 1) y, Pos (x + 1) y, Pos x (y - 1), Pos x (y + 1)]

spec :: Spec
spec = do
  describe "Pos" $ do
    it "Compares Y before X" $ compare (Pos 1 0) (Pos 0 1) `shouldBe` LT
    it "Orders items on the same row" $ sort [Pos 1 0, Pos 0 0] `shouldBe` [Pos 0 0, Pos 1 0]
    it "Orders y before x" $ sort [Pos 1 1, Pos 0 0, Pos 0 1, Pos 1 0] `shouldBe` [Pos 0 0, Pos 1 0, Pos 0 1, Pos 1 1]
    it "Selects the right neighbors" $ neighbors (Pos 0 0) `shouldBe` sort [Pos 0 1, Pos 0 (-1), Pos 1 0, Pos (-1) 0]
  describe "[Node]/Path" $ do
    it "Prefers shorter paths to the same destination" $ do
      let p1 = [Pos 0 0, Pos 1 0, Pos 1 1]
      let p2 = [Pos 0 0, Pos 0 1, Pos 0 2, Pos 1 2, Pos 1 1]
      comparePath p1 p2 `shouldBe` LT
    it "Prefers three-length paths according to reading order" $ do
      let p1 = [Pos 0 0, Pos 1 0, Pos 1 1]
      let p2 = [Pos 0 0, Pos 0 1, Pos 1 1]
      comparePath p1 p2 `shouldBe` LT
