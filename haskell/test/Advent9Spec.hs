module Advent9Spec where

import qualified Advent9
import           Data.Foldable
import qualified Data.IntMap   as IntMap
import           Data.List
import qualified Data.Maybe    as Maybe
import qualified Debug.Trace   as Trace
import qualified Deque
import           Test.Hspec

data GameStep =
  GameStep Advent9.Game
           Int
           [Advent9.Marble]

spec :: Spec
spec = do
  describe "Game" $ do
    it "Starting state is proper" $
      let (GameStep _ selected circle) = head steps
       in do circle `shouldBe` [0]
             selected `shouldBe` 0
    it "state after turn 1 is proper" $
      let (GameStep _ selected circle) = steps !! 1
       in do circle `shouldBe` [0, 1]
             selected `shouldBe` 1
    it "state after turn 2 is proper" $
      let (GameStep _ selected circle) = steps !! 2
       in do circle `shouldBe` [0, 2, 1]
             selected `shouldBe` 1
    it "state after turn 3 is proper" $
      let (GameStep _ selected circle) = steps !! 3
       in do circle `shouldBe` [0, 2, 1, 3]
             selected `shouldBe` 3
    it "state after turn 4 is proper" $
      let (GameStep _ selected circle) = steps !! 4
       in do circle `shouldBe` [0, 4, 2, 1, 3]
             selected `shouldBe` 1
  describe "shiftUntilZero" $ do
    it "Works on initial list" $
      let (n, c) = shiftUntilZero $ Deque.fromList [0]
       in do n `shouldBe` 0
             toList c `shouldBe` [0]
    it "Works on two-item non-shifted list" $
      let (n, c) = shiftUntilZero $ Deque.fromList [0, 1]
       in do n `shouldBe` 0
             toList c `shouldBe` [0, 1]
    it "Works on two-item shifted list" $
      let (n, c) = shiftUntilZero $ Deque.fromList [1, 0]
       in do n `shouldBe` 1
             toList c `shouldBe` [0, 1]
    it "Works on three-item shifted list" $
      let (n, c) = shiftUntilZero $ Deque.fromList [1, 0, 2]
       in do n `shouldBe` 2
             toList c `shouldBe` [0, 2, 1]
  where
    game0@(Advent9.Game _ circle0 scores0) = Advent9.startingGame
    step0 = GameStep game0 0 $ toList circle0
    (_, stepsAfter1) =
      mapAccumL
        (\(GameStep game selected circle) _ ->
           let nextGame@(Advent9.Game _ c _) = Advent9.playSingleTurn game
               (nextSelected, nextCircle) = shiftUntilZero c
               nextGameStep = (GameStep nextGame nextSelected $ toList nextCircle)
            in (nextGameStep, nextGameStep))
        step0
        [0 .. 100]
    steps = step0 : stepsAfter1

-- Shift a circle until the first element is zero, returning the shift amount
shiftUntilZero' :: Advent9.Marble -> Advent9.Circle -> (Int, Advent9.Circle)
shiftUntilZero' originalHead circle
  | Maybe.fromJust (Deque.head circle) == 0 =
    (Maybe.fromJust $ Data.List.elemIndex originalHead $ toList circle, circle)
  | otherwise = shiftUntilZero' originalHead (Deque.shiftLeft circle)

shiftUntilZero :: Advent9.Circle -> (Int, Advent9.Circle)
shiftUntilZero circle = shiftUntilZero' (Maybe.fromJust $ Deque.head circle) circle
