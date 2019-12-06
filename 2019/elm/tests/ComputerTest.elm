module ComputerTest exposing (..)

import Array
import Computer exposing (..)
import Debug
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "Computer"
        [ describe "readOp"
            [ testReadOp (Just (Input 0)) [ 3, 0, 4, 0 ]
            , testReadOp (Just (Output 4)) [ 4, 0 ]
            , testReadOp (Just Halt) [ 99 ]
            , testReadOp (Just (Add 1 2 3)) [ 1, 1, 2, 3 ]
            , testReadOp (Just (Mul 1 2 3)) [ 2, 1, 2, 3 ]
            , testReadOp (Just (Mul 33 3 4)) [ 1002, 4, 3, 4, 33 ]
            , testReadOp (Just (Add 4 3 4)) [ 1101, 4, 3, 4, 33 ]
            , testReadOp (Just (Input 225)) [ 3, 225, 1, 225, 6, 6 ]
            , testReadOp Nothing [ 1, 225, 6, 6 ]
            ]
        ]


testReadOp : Maybe LogicalOp -> List Int -> Test
testReadOp expected mem =
    let
        testName =
            Debug.toString mem
    in
    test testName <| \_ -> Expect.equal expected (readOp (compWithMem mem))


compWithMem : List Int -> Computer
compWithMem mem =
    { memory = mem |> Array.fromList
    , iPtr = 0
    , input = 0
    , output = []
    }
