module Problem10Test exposing (..)

import Expect
import Problem10 exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Problem 10"
        [ describe "shortestWholeVector"
            [ test "Positive" <| \_ -> Expect.equal ( 1, 1 ) (shortestWholeVector ( 2, 2 ))
            , test "Positive 2" <| \_ -> Expect.equal ( 1, 2 ) (shortestWholeVector ( 4, 8 ))
            , test "Negative " <| \_ -> Expect.equal ( -2, -1 ) (shortestWholeVector ( -4, -2 ))
            ]
        , describe "vectorGroups"
            [ test "Included" <| \_ -> containSame [ [ ( 1, 1 ), ( 2, 2 ) ] ] (vectorGroups [ ( 1, 1 ), ( 2, 2 ) ])
            , test "Two groups" <| \_ -> containSame [ [ ( 1, 1 ), ( 2, 2 ) ], [ ( 3, 0 ) ] ] (vectorGroups [ ( 1, 1 ), ( 3, 0 ), ( 2, 2 ) ])
            , test "Neg values" <| \_ -> containSame [ [ ( -1, -1 ), ( -2, -2 ) ], [ ( 3, 0 ) ] ] (vectorGroups [ ( -1, -1 ), ( 3, 0 ), ( -2, -2 ) ])
            , test "X Axis" <| \_ -> containSame [ [ ( 0, 1 ), ( 0, 2 ) ] ] (vectorGroups [ ( 0, 1 ), ( 0, 2 ) ])
            , test "Opposite sides" <| \_ -> containSame [ [ ( -1, -1 ) ], [ ( 1, 1 ) ] ] (vectorGroups [ ( -1, -1 ), ( 1, 1 ) ])
            ]
        , describe "test input"
            [ test "1, 0" <| \_ -> Expect.equal 7 (countVisible input1 ( 1, 0 ))
            , test "0, 2" <| \_ -> Expect.equal 6 (countVisible input1 ( 0, 2 ))
            , test "maxVisible" <| \_ -> Expect.equal 8 (problemA input1)
            ]
        , test "Problem A" <| \_ -> Expect.equal 274 (problemA inputProblemA)
        ]


containSame : List (List comparable) -> List (List comparable) -> Expect.Expectation
containSame a b =
    Expect.equal (a |> List.map List.sort |> List.sort) (b |> List.map List.sort |> List.sort)


input1 =
    """
.#..#
.....
#####
....#
...##
""" |> String.trim |> Problem10.parseInput


inputProblemA =
    """
.##.#.#....#.#.#..##..#.#.
#.##.#..#.####.##....##.#.
###.##.##.#.#...#..###....
####.##..###.#.#...####..#
..#####..#.#.#..#######..#
.###..##..###.####.#######
.##..##.###..##.##.....###
#..#..###..##.#...#..####.
....#.#...##.##....#.#..##
..#.#.###.####..##.###.#.#
.#..##.#####.##.####..#.#.
#..##.#.#.###.#..##.##....
#.#.##.#.##.##......###.#.
#####...###.####..#.##....
.#####.#.#..#.##.#.#...###
.#..#.##.#.#.##.#....###.#
.......###.#....##.....###
#..#####.#..#..##..##.#.##
##.#.###..######.###..#..#
#.#....####.##.###....####
..#.#.#.########.....#.#.#
.##.#.#..#...###.####..##.
##...###....#.##.##..#....
..##.##.##.#######..#...#.
.###..#.#..#...###..###.#.
#..#..#######..#.#..#..#.#""" |> String.trim |> Problem10.parseInput



{-

   [
   [(0,0)],
   [(0,1)],
   [(0,2)],
   [(0,3)],
   [(0,4)],[(1,0)],[(4,4),(3,3),(2,2),(1,1)],[(2,4),(1,2)],[(1,3)],[(1,4)],[(2,0)],[(4,2),(2,1)],[(2,3)],[(3,0)],[(3,1)],[(3,2)],[(3,4)],[(4,0)],[(4,1)],[(4,3)]]
-}
