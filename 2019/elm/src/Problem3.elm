module Problem3 exposing (..)

import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser)
import Set
import Tuple
import Util


{-| A cardinal direction to help us navigate on the wire board.
-}
type Dir
    = Up
    | Down
    | Left
    | Right


{-| Parses to a given direction.
-}
dirParser : Parser Dir
dirParser =
    Parser.oneOf
        [ Parser.symbol "U" |> Parser.map (\_ -> Up)
        , Parser.symbol "D" |> Parser.map (\_ -> Down)
        , Parser.symbol "L" |> Parser.map (\_ -> Left)
        , Parser.symbol "R" |> Parser.map (\_ -> Right)
        ]


{-| A span of wire extending for a given length in the given direction.
-}
type alias Span =
    ( Dir, Int )


{-| Parses a given wire Span.
-}
spanParser : Parser Span
spanParser =
    Parser.succeed Tuple.pair
        |= dirParser
        |= Parser.int


{-| A list of spans.
-}
type alias Wire =
    List Span


{-| X, Y pairs of points.
-}
type alias Point =
    ( Int, Int )


{-| Returns the manhattan distance from the origin for this point.
-}
distanceFromOrigin : Point -> Int
distanceFromOrigin p =
    abs (Tuple.first p) + abs (Tuple.second p)


{-| Returns the next point in the given direction.
-}
nextPoint : Point -> Dir -> Point
nextPoint point dir =
    let
        ( x, y ) =
            point
    in
    case dir of
        Up ->
            ( x, y + 1 )

        Down ->
            ( x, y - 1 )

        Left ->
            ( x - 1, y )

        Right ->
            ( x + 1, y )


{-| Given a starting point and a span, return a list of points that that span goes over.
-}
followSpan : Point -> Span -> List Point
followSpan start span =
    let
        ( dir, len ) =
            span

        next =
            nextPoint start dir
    in
    -- zero-length wires start and end on a single point
    if len == 0 then
        [ start ]

    else
        start :: followSpan next ( dir, len - 1 )


{-| Follows a wire given the starting point, returning the list of points that all the spans visit.
-}
followWire : Wire -> List Point
followWire spans =
    let
        -- When two spans meet in a corner, followWireFromPoints duplicates the corner. Here we remove them.
        -- Note that we also assume that the origin point is an "elbow" and remove it here.
        removeElbows lists =
            lists |> List.map List.tail |> Util.concatMaybes

        followWireFromPoint start recSpans =
            case recSpans of
                -- Empty wire (zero spans) does not have any points
                [] ->
                    []

                curSpan :: tail ->
                    let
                        points =
                            followSpan start curSpan

                        lastPoint =
                            points |> List.drop ((points |> List.length) - 1) |> List.head |> Maybe.withDefault start
                    in
                    points :: followWireFromPoint lastPoint tail
    in
    followWireFromPoint ( 0, 0 ) spans |> removeElbows |> List.concat


{-| Given a wire, return a list of Point+length pairs indicating the length of the wire at the given point. Given
that a wire can reach a point multiple times, we indicate the wire length that is the shortest at that point.
-}
wireLengthsAtPoints : Wire -> Dict Point Int
wireLengthsAtPoints wire =
    let
        pointDict =
            followWire wire
                -- Note that we add 1 to the length because we want to re-include the origin point which was omitted.
                |> List.indexedMap (\len point -> ( point, len + 1 ))
                -- Note that we must reverse in order to overwrite keys with *smaller* wire lengths
                |> List.reverse
                |> Dict.fromList
    in
    pointDict


{-| Given two wires, return the list of points in which they intersect.
-}
intersections : ( Wire, Wire ) -> List Point
intersections wires =
    let
        ( p1, p2 ) =
            Tuple.mapBoth followWire followWire wires
    in
    Set.intersect (Set.fromList p1) (Set.fromList p2) |> Set.toList


{-| Returns the distance to the nearest intersection.
-}
smallestIntersection : ( Wire, Wire ) -> Maybe Int
smallestIntersection wires =
    intersections wires |> List.map distanceFromOrigin |> List.minimum


smallestLengthBetweenIntersections : ( Wire, Wire ) -> Maybe Int
smallestLengthBetweenIntersections wires =
    let
        ( lengths1, lengths2 ) =
            Tuple.mapBoth wireLengthsAtPoints wireLengthsAtPoints wires

        unsafeGet key dict =
            case dict |> Dict.get key of
                Nothing ->
                    Debug.todo "Could not find key in dict"

                Just a ->
                    a

        lengthAtPoint point =
            (lengths1 |> unsafeGet point) + (lengths2 |> unsafeGet point)

        betweenLenghts =
            intersections wires |> List.map lengthAtPoint
    in
    betweenLenghts |> List.minimum


{-| Wire parser is a sequence of comma-separated spans.
-}
wireParser : Parser Wire
wireParser =
    Parser.sequence
        { start = ""
        , separator = ","
        , item = spanParser
        , end = ""
        , spaces = Parser.symbol ""
        , trailing = Parser.Forbidden
        }


{-| Parses the puzzle input to a pair of wires.
-}
twoWireParser : Parser ( Wire, Wire )
twoWireParser =
    Parser.succeed Tuple.pair
        |= wireParser
        |. Parser.symbol "\n"
        |= wireParser
        |. Parser.end


problemA : Parser (Maybe Int)
problemA =
    twoWireParser |> Parser.map smallestIntersection


problemB : Parser (Maybe Int)
problemB =
    twoWireParser |> Parser.map smallestLengthBetweenIntersections
