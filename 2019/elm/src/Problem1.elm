module Problem1 exposing (..)

import List
import String
import Util


{-| Fuel required to launch a given module is based on its mass. Specifically, to find the fuel required for a module,
take its mass, divide by three, round down, and subtract 2.
-}
fuelRequired : Int -> Int
fuelRequired mass =
    mass // 3 - 2


{-| Recursively calculate the fuel required assuming added fuel requires more fuel.
-}
recFuelRequired : Int -> Int
recFuelRequired mass =
    let
        newFuel =
            fuelRequired mass
    in
    if newFuel < 0 then
        0

    else
        newFuel + recFuelRequired newFuel


{-| Solution for the part A of the puzzle.
-}
puzzleA : String -> Int
puzzleA input =
    input
        |> Util.readInts
        |> List.map fuelRequired
        |> List.foldl (+) 0


{-| Solution for the part A of the puzzle.
-}
puzzleB : String -> Int
puzzleB input =
    input
        |> Util.readInts
        |> List.map recFuelRequired
        |> List.foldl (+) 0
