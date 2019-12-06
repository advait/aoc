module Problem6 exposing (..)

import Debug
import Parser exposing ((|.), (|=), Parser)
import Set exposing (Set)
import Tuple
import Util


type alias Planet =
    String


type alias Orbit =
    ( Planet, Planet )


{-| Returns a planet's parent.
-}
getParent : List Orbit -> Planet -> Maybe Planet
getParent orbits planet =
    orbits
        |> List.filter (\orbit -> Tuple.second orbit == planet)
        |> List.map Tuple.first
        |> List.head


{-| Returns a planet's children.
-}
getChildren : List Orbit -> Planet -> List Planet
getChildren orbits planet =
    orbits
        |> List.filter (\orbit -> Tuple.first orbit == planet)
        |> List.map Tuple.second


{-| Returns a planet's parent and children.
-}
getNeighbors : List Orbit -> Planet -> List Planet
getNeighbors orbits planet =
    (getParent orbits planet |> Util.listFromMaybe) ++ getChildren orbits planet


{-| Recursively computes the number of ancestors that a planet has.
-}
numAncestors : List Orbit -> Planet -> Int
numAncestors orbits planet =
    case getParent orbits planet of
        Nothing ->
            0

        Just parent ->
            1 + numAncestors orbits parent


{-| Runs breadth first search given the list of orbits returning the distance between the two
planets if a path exists.
-}
bfs : List Orbit -> Planet -> Planet -> Maybe Int
bfs orbits from dest =
    let
        rec : Int -> Set Planet -> Planet -> Maybe Int
        rec curStep visited cur =
            let
                notVisited p =
                    not (visited |> Set.member p)

                neighbors =
                    getNeighbors orbits cur |> List.filter notVisited

                nextVisitedSet =
                    visited |> Set.insert cur
            in
            if cur == dest then
                Just curStep

            else
                neighbors
                    |> List.map (\n -> rec (curStep + 1) nextVisitedSet n)
                    |> Util.concatMaybes
                    |> List.head
    in
    rec 0 Set.empty from


problemA : List Orbit -> Int
problemA orbits =
    orbits
        |> List.map Tuple.second
        |> List.map (numAncestors orbits)
        |> List.sum


problemB : List Orbit -> Maybe Int
problemB orbits =
    -- Note that we must subtract two because we want the distance between the *planets*
    -- not the distance between YOU and SAN.
    bfs orbits "YOU" "SAN" |> Maybe.map (\d -> d - 2)


planetParser : Parser Planet
planetParser =
    Parser.getChompedString <|
        Parser.succeed ()
            |. Parser.chompWhile (\c -> c /= ')' && c /= '\n')


orbitParser : Parser Orbit
orbitParser =
    Parser.succeed Tuple.pair
        |= planetParser
        |. Parser.symbol ")"
        |= planetParser


orbitsParser : Parser (List Orbit)
orbitsParser =
    Parser.sequence
        { start = ""
        , separator = "\n"
        , end = ""
        , spaces = Parser.symbol ""
        , item = orbitParser
        , trailing = Parser.Optional
        }
