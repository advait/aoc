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


{-| Return all unique the two-tuple permutations of items in the input, allowing for duplicates.
-}
permutations2 : List a -> List ( a, a )
permutations2 input =
    input |> List.concatMap (\first -> input |> List.map (\second -> ( first, second )))


{-| Given a sorted list of items, return a list of groups of consecutive items.
-}
groups : List a -> List (List a)
groups input =
    let
        rec curItem curGroup remaining =
            case remaining of
                [] ->
                    [ curGroup ]

                head :: tail ->
                    if head == curItem then
                        rec curItem (head :: curGroup) tail

                    else
                        curGroup :: rec head [ head ] tail
    in
    case input of
        [] ->
            []

        head :: tail ->
            rec head [ head ] tail
