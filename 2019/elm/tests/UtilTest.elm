module UtilTest exposing (..)

import Expect
import Parser exposing (Parser)


{-| Executes the parser against the given input string, asserting that it matches the output.
-}
expectParse : Parser a -> String -> a -> Expect.Expectation
expectParse parser input expected =
    case Parser.run parser input of
        Result.Ok output ->
            Expect.equal expected output

        Result.Err deadEnds ->
            Expect.fail (deadEnds |> Parser.deadEndsToString)
