module Problem1Test exposing (..)

import Expect exposing (Expectation)
import Problem1
import Test exposing (..)


suite : Test
suite =
    describe "Problem 1"
        [ test "a" <| \_ -> Expect.equal 3239503 (Problem1.puzzleA input)
        , test "b" <| \_ -> Expect.equal 4856390 (Problem1.puzzleB input)
        , describe "b test cases"
            [ test "1" <| \_ -> Expect.equal 2 (Problem1.recFuelRequired 14)
            , test "2" <| \_ -> Expect.equal 966 (Problem1.recFuelRequired 1969)
            , test "3" <| \_ -> Expect.equal 50346 (Problem1.recFuelRequired 100756)
            ]
        ]


input =
    """
    106947
    129138
    56893
    75116
    96763
    108475
    62574
    137915
    73556
    69652
    74098
    131265
    77267
    72940
    74859
    128578
    128024
    125887
    140457
    97314
    126150
    148019
    116715
    54231
    98892
    73242
    131621
    122572
    107437
    75142
    103755
    141267
    141024
    80452
    60619
    104099
    51541
    63863
    106932
    75346
    77073
    57263
    128967
    124504
    79388
    124167
    100073
    97108
    74180
    137778
    73793
    131458
    67579
    102695
    143794
    96093
    64490
    96122
    88901
    53381
    77850
    96527
    51943
    107511
    120126
    64622
    63053
    116916
    83990
    143278
    72390
    101767
    135915
    126354
    109714
    56139
    129849
    89505
    115213
    145001
    56506
    83700
    59071
    80895
    143177
    120738
    78270
    100436
    108389
    62456
    145335
    102210
    111779
    95937
    52626
    134932
    61925
    97086
    50211
    96413
    """
