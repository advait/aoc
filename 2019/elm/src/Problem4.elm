module Problem4 exposing (..)

import List
import Util


{-| Returns whether you have at least two adjacent items in the list that are the same.
-}
atLeastTwoAdjacent : List a -> Bool
atLeastTwoAdjacent list =
    list |> Util.groups |> List.map List.length |> List.any (\num -> num >= 2)


{-| Returns whether you have exactly two adjacent items in the list that are the same.
-}
exactlyTwoAdjacent : List a -> Bool
exactlyTwoAdjacent list =
    list |> Util.groups |> List.map List.length |> List.any (\num -> num == 2)


{-| Returns whether you the numbers in the list never decrease.
-}
neverDecrease : List comparable -> Bool
neverDecrease inputList =
    case inputList of
        a :: b :: tail ->
            if b < a then
                False

            else
                neverDecrease (b :: tail)

        _ ->
            True


{-| Given an integer, convert it into a list of decimal digits.
-}
intToList : Int -> List Int
intToList num =
    num
        |> String.fromInt
        |> String.toList
        |> List.map String.fromChar
        |> List.map String.toInt
        |> Util.concatMaybes


startList =
    List.range 134564 585159 |> List.map intToList


problemA =
    startList
        |> List.filter (\x -> atLeastTwoAdjacent x && neverDecrease x)
        |> List.length


problemB =
    startList
        |> List.filter (\x -> exactlyTwoAdjacent x && neverDecrease x)
        |> List.length
