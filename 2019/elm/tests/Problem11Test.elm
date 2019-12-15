module Problem11Test exposing (..)

import Basics
import Expect
import Problem11 exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Problem 11"
        [ describe "simGravity"
            [ test "A" <| \_ -> Expect.equal ( -1, 1, 1 ) (simGravity1 ( 1, 2, 3 ) ( 0, 3, 4 ))
            , test "B" <| \_ -> Expect.equal ( 1, 0, 1 ) (simGravity1 ( 1, 2, 3 ) ( 2, 2, 5 ))
            , test "C" <| \_ -> Expect.equal ( 0, 0, 0 ) (simGravity1 ( 1, 2, 3 ) ( 1, 2, 3 ))
            ]
        , describe "step"
            [ test "t1" <| \_ -> Expect.equal t1 (step t0)
            ]
        , describe "energy"
            [ test "t0" <| \_ -> Expect.equal 0 (totalEnergy t0)

            --, test "t1" <| \_ -> Expect.equal 0 (totalEnergy t1)
            --, test "t3" <| \_ -> Expect.equal 179 (totalEnergy ( [ ( 2, -2, 1 ), ( 1, -4, -4 ), ( 3, -7, 5 ), ( 2, 0, 0 ) ], [ ( 3, 5, -2 ), ( -2, -4, -4 ), ( 0, -5, 4 ), ( -1, 4, 2 ) ] ))
            ]
        , test "hint1" <| \_ -> Expect.equal 179 (repeat step 10 t0 |> totalEnergy)
        , test "Problem A" <| \_ -> Expect.equal 179 (repeat step 1000 puzzle0 |> totalEnergy)
        ]


t0 =
    ( [ ( -1, 0, 2 ), ( 2, -10, -7 ), ( 4, -8, 8 ), ( 3, 5, -1 ) ], [ ( 0, 0, 0 ), ( 0, 0, 0 ), ( 0, 0, 0 ), ( 0, 0, 0 ) ] )


t1 =
    ( [ ( 2, -1, 1 ), ( 3, -7, -4 ), ( 1, -7, 5 ), ( 2, 2, 0 ) ], [ ( 3, -1, -1 ), ( 1, 3, 3 ), ( -3, 1, -3 ), ( -1, -3, 1 ) ] )


puzzle0 =
    ( [ ( -3, 0, 0 ), ( -12, 0, 0 ), ( -9, 0, 0 ), ( 7, 0, 0 ) ], [ ( 0, 0, 0 ), ( 0, 0, 0 ), ( 0, 0, 0 ), ( 0, 0, 0 ) ] )



--( [ ( -3, 10, -1 ), ( -12, -10, -5 ), ( -9, 0, 10 ), ( 7, -5, -3 ) ], [ ( 0, 0, 0 ), ( 0, 0, 0 ), ( 0, 0, 0 ), ( 0, 0, 0 ) ] )
