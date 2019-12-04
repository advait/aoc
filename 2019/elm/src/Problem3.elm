module Problem3 exposing (..)

import Parser exposing ((|.), (|=), Parser)


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
        , Parser.symbol "D" |> Parser.map (\_ -> Right)
        ]


{-| A span of wire extending for a given length in the given direction.
-}
type alias Span =
    ( Dir, Int )


{-| Parses a given wire Span
-}
spanParser : Parser Span
spanParser =
    Parser.succeed (\dir -> \len -> ( dir, len ))
        |= dirParser
        |= Parser.int


type alias Wire =
    List Span


{-| Wire parser is a sequence of comma-separated spans ending with a newline.
-}
wireParser : Parser Wire
wireParser =
    Parser.sequence { start = "", separator = ",", item = spanParser, end = "", spaces = Parser.symbol "", trailing = Parser.Forbidden }
