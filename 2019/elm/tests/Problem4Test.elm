module Problem4Test exposing (..)

import Expect
import Problem4
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Problem 4"
        [ Test.test "Part A" <| \_ -> Expect.equal 1929 Problem4.problemA
        , Test.test "Part B" <| \_ -> Expect.equal 1306 Problem4.problemB
        ]
