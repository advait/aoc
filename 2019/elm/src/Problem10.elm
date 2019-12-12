module Problem10 exposing (..)

import Basics
import Compare
import Dict exposing (Dict)
import Util


{-| A point in X, Y space.
-}
type alias Point =
    ( Int, Int )


{-| A vector starting form the origin to the given point.
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


{-| Return the magnitude of the vector.
-}
mag : Vector -> Float
mag v =
    let
        ( x, y ) =
            v
    in
    sqrt (toFloat x ^ 2 + toFloat y ^ 2)


{-| Angle between (0, -1) and the given point.
-}
angleFromUp : Vector -> Float
angleFromUp =
    Tuple.mapBoth toFloat toFloat
        >> Basics.toPolar
        >> Tuple.second
        >> (+) (Basics.pi / 2)
        >> normalizeAngle


{-| Normalizes an angle so that it ends up between 0 and 2\*pi.
-}
normalizeAngle : Float -> Float
normalizeAngle a =
    let
        twoPi =
            Basics.pi * 2
    in
    if a < 0 then
        normalizeAngle (a + twoPi)

    else if a > twoPi then
        normalizeAngle (a - twoPi)

    else
        a


{-| Shorten the given vector so that its components are the smallest integers possible.
-}
shortestWholeVector : Vector -> Vector
shortestWholeVector v =
    let
        ( deltaX, deltaY ) =
            v

        divisor =
            Util.gcf (abs deltaX) (abs deltaY)
    in
    ( deltaX // divisor, deltaY // divisor )


{-| Returns groups of vectors based on which "shortestWholeVector" group they belong to.
-}
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
            asteroids |> Dict.keys |> List.map (\p -> liftOp (-) p point)

        isAsteroid : Point -> Bool
        isAsteroid loc =
            Dict.get (liftOp (+) point loc) asteroids == Just True

        groups =
            vecs
                |> vectorGroups
                |> List.filter
                    (List.any isAsteroid)
    in
    (groups |> List.length) - 1


{-| Sorts the asteroids based on which order they would be zapped by a laser at the given point.
-}
sortByLaser : Point -> List Point -> List Point
sortByLaser point asteroids =
    asteroids
        |> List.map (\p -> liftOp (-) p point)
        |> List.sortWith (Compare.by angleFromUp)
        |> Util.groupsBy (Compare.by angleFromUp)
        |> List.map (List.sortBy mag)
        |> cycleTakeFromGroups
        |> List.map (liftOp (+) point)


problemA : Dict Point Bool -> ( Int, Point )
problemA asteroids =
    asteroids
        |> Dict.filter (\_ val -> val)
        |> Dict.keys
        |> List.map (\p -> ( countVisible asteroids p, p ))
        |> List.maximum
        |> Maybe.withDefault ( 0, ( 0, 0 ) )


{-| Given a list of lists, consume a single item from each inner list, until the whole outer list is consumed.
-}
cycleTakeFromGroups : List (List a) -> List a
cycleTakeFromGroups groups =
    case groups of
        [] ->
            []

        [] :: remaining ->
            cycleTakeFromGroups remaining

        (head :: tail) :: remaining ->
            head :: cycleTakeFromGroups (remaining ++ [ tail ])


problemB : Dict Point Bool -> Maybe Point
problemB allPoints =
    let
        ( _, point ) =
            problemA allPoints

        asteroids =
            allPoints
                |> Dict.filter (\_ val -> val)
                |> Dict.keys
    in
    asteroids
        |> sortByLaser point
        |> List.drop (200 - 1)
        |> List.head
