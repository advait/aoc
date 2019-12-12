module Problem10Test exposing (..)

import Basics
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
            [ test "1, 0" <| \_ -> Expect.equal 7 (countVisible testInput1 ( 1, 0 ))
            , test "0, 2" <| \_ -> Expect.equal 6 (countVisible testInput1 ( 0, 2 ))
            , test "maxVisible" <| \_ -> Expect.equal ( 8, ( 3, 4 ) ) (problemA testInput1)
            ]
        , describe "angleFomUp"
            [ test "mag" <| \_ -> Expect.within tolerance (sqrt 2) (mag ( -1, 1 ))
            , test "0, -1" <| \_ -> Expect.within tolerance 0 (angleFromUp ( 0, -1 ))
            , test "0, -2" <| \_ -> Expect.within tolerance 0 (angleFromUp ( 0, -2 ))
            , test "1, 0" <| \_ -> Expect.within tolerance (Basics.pi / 2) (angleFromUp ( 1, 0 ))
            , test "0, 1" <| \_ -> Expect.within tolerance Basics.pi (angleFromUp ( 0, 1 ))
            , test "-1, 0" <| \_ -> Expect.within tolerance (3 * Basics.pi / 2) (angleFromUp ( -1, 0 ))
            , test "-1, -1" <| \_ -> Expect.within tolerance (7 * Basics.pi / 4) (angleFromUp ( -1, -1 ))
            ]
        , describe "laserOrdering"
            [ test "A" <| \_ -> Expect.equal [ ( 0, -1 ), ( 1, 0 ), ( 0, 1 ), ( -1, 0 ) ] (sortByLaser ( 0, 0 ) [ ( 1, 0 ), ( 0, -1 ), ( 0, 1 ), ( -1, 0 ) ])
            , test "B" <| \_ -> Expect.equal [ ( 0, -1 ), ( 1, 0 ), ( 0, 1 ), ( -1, 0 ), ( 0, -2 ), ( 2, 0 ) ] (sortByLaser ( 0, 0 ) [ ( 0, -1 ), ( 1, 0 ), ( 0, 1 ), ( -1, 0 ), ( 0, -2 ), ( 2, 0 ) ])
            , test "C" <| \_ -> Expect.equal [ ( 0, -1 ), ( 1, -4 ), ( 1, 0 ), ( 0, 1 ), ( -1, 0 ), ( 0, -2 ), ( 2, 0 ) ] (sortByLaser ( 0, 0 ) [ ( 0, -1 ), ( 1, 0 ), ( 0, 1 ), ( -1, 0 ), ( 0, -2 ), ( 2, 0 ), ( 1, -4 ) ])
            , test "D" <| \_ -> Expect.equal [ ( 1, 0 ), ( 0, 1 ), ( 0, 0 ) ] (sortByLaser ( 1, 1 ) [ ( 1, 0 ), ( 0, 1 ), ( 0, 0 ) ])
            ]
        , test "Problem A" <| \_ -> Expect.equal ( 274, ( 19, 14 ) ) (problemA realInput)
        , test "Problem B" <| \_ -> Expect.equal (Just ( 3, 5 )) (problemB realInput)
        ]


tolerance =
    Expect.Absolute 0.00001


containSame : List (List comparable) -> List (List comparable) -> Expect.Expectation
containSame a b =
    Expect.equal (a |> List.map List.sort |> List.sort) (b |> List.map List.sort |> List.sort)


testInput1 =
    """
.#..#
.....
#####
....#
...##
""" |> String.trim |> Problem10.parseInput


realInput =
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
