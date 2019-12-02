module Advent12Spec where

import           A12.Advent12
import           Data.Foldable
import qualified Data.IntMap   as IntMap
import           Data.List
import qualified Data.Maybe    as Maybe
import qualified Debug.Trace   as Trace
import qualified Deque
import           Test.Hspec

spec :: Spec
spec =
  describe "pad" $ do
    it "Pads single plant" $ pad 5 (0, "#") `shouldBe` (5, ".....#.....")
    it "Unpads if extra" $ pad 5 (1, ".#.") `shouldBe` (5, ".....#.....")
    it "Unpads if a lot extra" $ pad 5 (8, "......#.#.#......") `shouldBe` (7, ".....#.#.#.....")
