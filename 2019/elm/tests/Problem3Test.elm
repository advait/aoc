module Problem3Test exposing (..)

import Expect
import Parser
import Problem3 exposing (Dir(..))
import Result
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Problem 3"
        [ Test.describe "parsers"
            [ Test.test "wire parser parses empty" <| \_ -> expectParsedWire [] ""
            , Test.test "wire parser parses one item" <| \_ -> expectParsedWire [ ( Down, 7 ) ] "D7"
            , Test.test "wire parser parses two items" <| \_ -> expectParsedWire [ ( Down, 7 ), ( Up, 0 ) ] "D7,U0"
            ]
        ]


expectParsedWire : Problem3.Wire -> String -> Expect.Expectation
expectParsedWire wire1 input =
    case Parser.run Problem3.wireParser input of
        Result.Ok wire2 ->
            Expect.equal wire1 wire2

        Result.Err deadEnds ->
            Expect.fail (deadEnds |> Parser.deadEndsToString)
