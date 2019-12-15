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
            [ testReadOp (Just ( InputOp, [ ( Dest, Position, 0 ) ] )) [ 3, 0, 4, 0 ]
            , testReadOp (Just ( OutputOp, [ ( Arg, Position, 3 ) ] )) [ 4, 3 ]
            , testReadOp (Just ( HaltOp, [] )) [ 99 ]
            , testReadOp (Just ( AddOp, [ ( Arg, Position, 1 ), ( Arg, Position, 2 ), ( Dest, Position, 3 ) ] )) [ 1, 1, 2, 3 ]
            , testReadOp (Just ( MulOp, [ ( Arg, Position, 1 ), ( Arg, Position, 2 ), ( Dest, Position, 3 ) ] )) [ 2, 1, 2, 3 ]
            , testReadOp (Just ( AddOp, [ ( Arg, Position, 26 ), ( Arg, Immediate, -4 ), ( Dest, Position, 26 ) ] )) [ 1001, 26, -4, 26 ]
            , testReadOp (Just ( MulOp, [ ( Arg, Position, 4 ), ( Arg, Immediate, 3 ), ( Dest, Position, 4 ) ] )) [ 1002, 4, 3, 4, 33 ]
            , testReadOp (Just ( AddOp, [ ( Arg, Immediate, 4 ), ( Arg, Immediate, 3 ), ( Dest, Position, 4 ) ] )) [ 1101, 4, 3, 4, 33 ]
            , testReadOp (Just ( AddOp, [ ( Arg, Immediate, 4 ), ( Arg, Immediate, 3 ), ( Dest, BaseRelative, 4 ) ] )) [ 21101, 4, 3, 4, 33 ]
            , testReadOp (Just ( AddOp, [ ( Arg, BaseRelative, 4 ), ( Arg, BaseRelative, 3 ), ( Dest, BaseRelative, 4 ) ] )) [ 22201, 4, 3, 4, 33 ]
            , testReadOp (Just ( InputOp, [ ( Dest, Position, 225 ) ] )) [ 3, 225, 1, 225, 6, 6 ]
            , testReadOp (Just ( AdjustRelativeBaseOp, [ ( Arg, Immediate, 1 ) ] )) [ 109, 1, 204, -1 ]
            , testReadOp Nothing [ 29, 225, 6, 6 ]
            ]
        , describe "Sample Programs"
            [ testProgram "3,9,8,9,10,9,4,9,99,-1,8" [ 8 ] [ 1 ] -- Using position mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not).
            , testProgram "3,9,8,9,10,9,4,9,99,-1,8" [ 9 ] [ 0 ] -- Using position mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not).
            , testProgram "3,9,7,9,10,9,4,9,99,-1,8" [ 6 ] [ 1 ] -- Using position mode, consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not).
            , testProgram "3,9,7,9,10,9,4,9,99,-1,8" [ 9 ] [ 0 ] -- Using position mode, consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not).
            , testProgram "3,3,1108,-1,8,3,4,3,99" [ 8 ] [ 1 ] -- Using immediate mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not).
            , testProgram "3,3,1108,-1,8,3,4,3,99" [ 9 ] [ 0 ] -- Using immediate mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not).
            , testProgram "3,3,1107,-1,8,3,4,3,99" [ 7 ] [ 1 ] -- Using immediate mode, consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not).
            , testProgram "3,3,1107,-1,8,3,4,3,99" [ 9 ] [ 0 ] -- Using immediate mode, consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not).

            -- Here are some jump tests that take an input, then output 0 if the input was zero or 1 if the input was non-zero:
            , testProgram "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" [ 0 ] [ 0 ]
            , testProgram "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" [ 9 ] [ 1 ]
            , testProgram "3,3,1105,-1,9,1101,0,0,12,4,12,99,1" [ 0 ] [ 0 ]
            , testProgram "3,3,1105,-1,9,1101,0,0,12,4,12,99,1" [ 9 ] [ 1 ]

            -- The below example program uses an input instruction to ask for a single number. The program will then output 999 if the input value is below 8, output 1000 if the input value is equal to 8, or output 1001 if the input value is greater than 8.
            , testProgram "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99" [ 7 ] [ 999 ]
            , testProgram "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99" [ 8 ] [ 1000 ]
            , testProgram "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99" [ 9 ] [ 1001 ]

            -- takes no input and produces a copy of itself as output.
            , testProgram "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99" [] [ 109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99 ]

            -- should output a 16-digit number.
            , testProgram "1102,34915192,34915192,7,4,7,99,0" [] [ 1219070632396864 ]

            -- should output the large number in the middle.
            , testProgram "104,1125899906842624,99" [] [ 1125899906842624 ]
            ]
        ]


testReadOp : Maybe LogicalOp -> List Int -> Test
testReadOp expected mem =
    let
        testName =
            Debug.toString mem
    in
    test testName <| \_ -> Expect.equal expected (readOp (compWithMem mem))


testProgram : String -> List Int -> List Int -> Test
testProgram program inputs expectedOutput =
    let
        testName =
            "Program " ++ program ++ ", input" ++ Debug.toString inputs

        finalComp =
            Computer.fromString program
                |> Computer.withInputs inputs
                |> Computer.execUntilHalt
    in
    test testName <| \_ -> Expect.equal expectedOutput finalComp.output


compWithMem : List Int -> Computer
compWithMem mem =
    { memory = mem |> Array.fromList
    , iPtr = 0
    , basePtr = 0
    , inputs = []
    , output = []
    }
