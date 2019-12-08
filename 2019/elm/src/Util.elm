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


{-| Converts a Maybe into a singleton or empty list.
-}
listFromMaybe : Maybe a -> List a
listFromMaybe maybe =
    case maybe of
        Nothing ->
            []

        Just a ->
            [ a ]


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


{-| Unsafely returns the head of a list.
-}
unsafeHead : List a -> a
unsafeHead list =
    case list of
        head :: _ ->
            head

        _ ->
            Debug.todo "error, no head"


{-| Simultaneously performs a map and foldl operation.
-}
mapAndFoldl : (a -> b -> ( c, b )) -> b -> List a -> ( List c, b )
mapAndFoldl f acc list =
    case list of
        [] ->
            ( [], acc )

        head :: tail ->
            let
                ( mappedItem, output ) =
                    f head acc

                ( remainingItems, finalOutput ) =
                    mapAndFoldl f output tail
            in
            ( mappedItem :: remainingItems, finalOutput )


{-| Return all unique the two-tuple permutations of items in the input, allowing for duplicates.
-}
permutations2 : List a -> List ( a, a )
permutations2 input =
    input |> List.concatMap (\first -> input |> List.map (\second -> ( first, second )))


{-| Return all combinations in the form of (element, rest of the list). Read [Haskell Libraries proposal](https://mail.haskell.org/pipermail/libraries/2008-February/009270.html) for further ideas on how to use this function.
select [1,2,3,4] == [(1,[2,3,4]),(2,[1,3,4]),(3,[1,2,4]),(4,[1,2,3])]
-}
select : List a -> List ( a, List a )
select items =
    case items of
        [] ->
            []

        x :: xs ->
            ( x, xs ) :: List.map (\( y, ys ) -> ( y, x :: ys )) (select xs)


{-| Return all unique permutations of the given list (order matters, all items selected).
-}
permutations : List a -> List (List a)
permutations xs_ =
    case xs_ of
        [] ->
            [ [] ]

        xs ->
            let
                f ( y, ys ) =
                    List.map ((::) y) (permutations ys)
            in
            List.concatMap f (select xs)


{-| Converts True to 1 and False to 0.
-}
boolToInt : Bool -> Int
boolToInt bool =
    case bool of
        True ->
            1

        False ->
            0


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
