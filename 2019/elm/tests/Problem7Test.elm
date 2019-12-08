module Problem7Test exposing (..)

import Expect
import Problem7
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Problem 7"
        [ Test.test "Part A" <| \_ -> Expect.equal (Just 38500) Problem7.problemA
        , Test.test "Part B" <| \_ -> Expect.equal (Just 33660560) Problem7.problemB
        ]
