module Problem10 exposing (..)

import Compare
import Dict exposing (Dict)
import Set
import Util


{-| A point in X, Y space.
-}
type alias Point =
    ( Int, Int )


{-| A vector starting form the origin.
-}
type alias Vector =
    Point


{-| Lifts an operation on integers to an operation on points.
-}
liftOp : (Int -> Int -> Int) -> (Point -> Point -> Point)
liftOp op p0 p1 =
    let
        ( ( x0, y0 ), ( x1, y1 ) ) =
            ( p0, p1 )
    in
    ( op x0 x1, op y0 y1 )


minus =
    liftOp (-)


plus =
    liftOp (+)


{-| Shorten the given vector by
-}
shortestWholeVector : Vector -> Vector
shortestWholeVector v =
    let
        ( deltaX, deltaY ) =
            v

        divisor =
            gcf (abs deltaX) (abs deltaY)
    in
    ( deltaX // divisor, deltaY // divisor )


{-| Returns the integer factors for the given number.
-}
factors : Int -> List Int
factors n =
    List.range 1 n
        |> List.filter (\f -> modBy f n == 0)


{-| Returns the greatest common factor between the two integers.
-}
gcf : Int -> Int -> Int
gcf x y =
    case ( x, y ) of
        ( 0, _ ) ->
            y

        ( _, 0 ) ->
            x

        _ ->
            Set.intersect (factors x |> Set.fromList) (factors y |> Set.fromList)
                |> Set.toList
                |> List.maximum
                |> Maybe.withDefault 1


vectorGroups : List Vector -> List (List Vector)
vectorGroups vecs =
    let
        -- First shortens then compares vectors.
        compareVec : Vector -> Vector -> Order
        compareVec =
            Compare.concat
                [ Compare.compose shortestWholeVector compare
                ]
    in
    vecs
        |> List.sortWith compareVec
        |> Util.groupsBy compareVec


{-| Parses the input, returning a dictionary of boolean asteroid locations.
-}
parseInput : String -> Dict Point Bool
parseInput input =
    input
        |> String.trim
        |> String.split "\n"
        |> List.indexedMap
            (\y row ->
                row
                    |> String.toList
                    |> List.indexedMap
                        (\x char ->
                            ( ( x, y ), char == '#' )
                        )
            )
        |> List.concat
        |> Dict.fromList


{-| Counts the number of visible asteroids from the given point.
-}
countVisible : Dict Point Bool -> Point -> Int
countVisible asteroids point =
    let
        vecs =
            asteroids |> Dict.keys |> List.map (\p -> minus p point)

        groups =
            vecs
                |> vectorGroups
                |> List.filter
                    (\group ->
                        group
                            |> List.any
                                (\loc ->
                                    Dict.get (plus point loc) asteroids == Just True
                                )
                    )
    in
    (groups |> List.length) - 1


problemA : Dict Point Bool -> Int
problemA asteroids =
    asteroids
        |> Dict.filter (\_ val -> val)
        |> Dict.keys
        |> List.map (countVisible asteroids)
        |> List.maximum
        |> Maybe.withDefault 0
