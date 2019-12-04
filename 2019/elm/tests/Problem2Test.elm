module Problem2Test exposing (..)

import Expect exposing (Expectation)
import Problem2
import Test exposing (..)


suite : Test
suite =
    describe "Problem 2"
        [ test "a" <| \_ -> Expect.equal 5482655 (Problem2.problemA modifiedInput)
        , describe "a test cases"
            [ test "1" <| \_ -> expectAfterExecHalt [ 2, 0, 0, 0, 99 ] "1,0,0,0,99"
            , test "2" <| \_ -> expectAfterExecHalt [ 2, 3, 0, 6, 99 ] "2,3,0,3,99"
            , test "3" <| \_ -> expectAfterExecHalt [ 2, 4, 4, 5, 99, 9801 ] "2,4,4,5,99,0"
            , test "4" <| \_ -> expectAfterExecHalt [ 30, 1, 1, 4, 2, 5, 6, 0, 99 ] "1,1,1,4,99,5,6,0,99"
            , test "5" <| \_ -> expectAfterExecHalt [ 30, 1, 1, 4, 2, 5, 6, 0, 99 ] "1,1,1,4,99,5,6,0,99"
            ]
        , test "b" <| \_ -> Expect.equal 4967 (Problem2.problemB 19690720 input)
        ]


{-| Executes all instructions given a start state and asserts that the memory was as given.
-}
expectAfterExecHalt finalMem start =
    Expect.equal finalMem (start |> Problem2.inputToComputer |> Problem2.execUntilHalt |> .memory)


input =
    "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,10,19,23,1,6,23,27,1,5,27,31,1,10,31,35,2,10,35,39,1,39,5,43,2,43,6,47,2,9,47,51,1,51,5,55,1,5,55,59,2,10,59,63,1,5,63,67,1,67,10,71,2,6,71,75,2,6,75,79,1,5,79,83,2,6,83,87,2,13,87,91,1,91,6,95,2,13,95,99,1,99,5,103,2,103,10,107,1,9,107,111,1,111,6,115,1,115,2,119,1,119,10,0,99,2,14,0,0"


modifiedInput =
    "1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,10,19,23,1,6,23,27,1,5,27,31,1,10,31,35,2,10,35,39,1,39,5,43,2,43,6,47,2,9,47,51,1,51,5,55,1,5,55,59,2,10,59,63,1,5,63,67,1,67,10,71,2,6,71,75,2,6,75,79,1,5,79,83,2,6,83,87,2,13,87,91,1,91,6,95,2,13,95,99,1,99,5,103,2,103,10,107,1,9,107,111,1,111,6,115,1,115,2,119,1,119,10,0,99,2,14,0,0"
