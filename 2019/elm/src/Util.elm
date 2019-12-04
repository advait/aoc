module Util exposing (..)

import List
import String


{-| Reads the input string, parses the lines as integers, and returns a List Int.
-}
readInts : String -> List Int
readInts input =
    input
        |> String.lines
        |> List.map String.trim
        |> List.map String.toInt
        |> concatMaybes


{-| Remove Nothings from a List of Maybes and unwraps the Justs.
-}
concatMaybes : List (Maybe a) -> List a
concatMaybes maybes =
    case maybes of
        [] ->
            []

        Nothing :: tail ->
            concatMaybes tail

        (Just a) :: tail ->
            a :: concatMaybes tail
