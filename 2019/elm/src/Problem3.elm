module Problem3 exposing (..)

import Parser exposing ((|.), (|=), Parser)
import Set


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
followWire : Point -> Wire -> List Point
followWire start spans =
    case spans of
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
            points ++ followWire lastPoint tail


{-| Returns the distance to the nearest intersection.
-}
intersection : ( Wire, Wire ) -> Maybe Int
intersection wires =
    let
        ( w1, w2 ) =
            wires

        p1 =
            followWire ( 0, 0 ) w1

        p2 =
            followWire ( 0, 0 ) w2

        intersections =
            Set.intersect (Set.fromList p1) (Set.fromList p2) |> Set.toList

        -- Note that the origin will always be the first point.
        secondItem list =
            case list of
                first :: second :: _ ->
                    Just second

                _ ->
                    Nothing

        smallestIntersection =
            intersections |> List.map distanceFromOrigin |> List.sort |> secondItem
    in
    smallestIntersection


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
    twoWireParser |> Parser.map intersection
