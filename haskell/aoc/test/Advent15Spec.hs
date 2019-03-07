module Advent15Spec where

import Advent15
import           Data.Foldable
import qualified Data.IntMap   as IntMap
import           Data.List
import qualified Data.Maybe    as Maybe
import qualified Debug.Trace   as Trace
import qualified Deque
import           Test.Hspec

spec :: Spec
spec = describe "Pos" $ do
  it "Compares Y before X" $
    compare (Pos 1 0) (Pos 0 1) `shouldBe` LT
  it "Orders items on the same row" $
    sort [Pos 1 0, Pos 0 0] `shouldBe` [Pos 0 0, Pos 1 0]
  it "Orders y before x" $
    sort [Pos 1 1, Pos 0 0, Pos 0 1, Pos 1 0] `shouldBe` [Pos 0 0, Pos 1 0, Pos 0 1, Pos 1 1]
