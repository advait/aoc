module Problem11 exposing (..)

import Set exposing (Set)
import Util


type alias Pos =
    ( Int, Int, Int )


{-| Represents a planet with a position and a velocity.
-}
type alias Planets =
    ( List Pos, List Pos )


addPos : Pos -> Pos -> Pos
addPos left right =
    let
        ( ( leftX, leftY, leftZ ), ( rightX, rightY, rightZ ) ) =
            ( left, right )
    in
    ( leftX + rightX, leftY + rightY, leftZ + rightZ )


{-| Simulates gravity for the left planet, returning the delta velocity for it.
-}
simGravity1 : Pos -> Pos -> Pos
simGravity1 left right =
    let
        ( ( leftX, leftY, leftZ ), ( rightX, rightY, rightZ ) ) =
            ( left, right )

        grav l r =
            case compare l r of
                LT ->
                    1

                EQ ->
                    0

                GT ->
                    -1
    in
    ( grav leftX rightX, grav leftY rightY, grav leftZ rightZ )


{-| Simulates the gravity for the left planet with respect to all other planets, returning a cumulative
delta velocity.
-}
simGravityAll : Pos -> List Pos -> Pos
simGravityAll left planets =
    planets
        |> List.map (\p -> simGravity1 left p)
        |> Util.reduce addPos


{-| Given a list of planet positions and velocities, run a time step, returning a list of new positions and velocities.
-}
step : Planets -> Planets
step planets =
    let
        ( positions, vels ) =
            planets

        deltaVels =
            positions |> List.map (\p -> simGravityAll p positions)

        newVels =
            List.map2 addPos vels deltaVels

        newPositions =
            List.map2 addPos positions newVels
    in
    ( newPositions, newVels )


totalEnergy : Planets -> Int
totalEnergy planets =
    let
        ( positions, vels ) =
            planets

        energy : Pos -> Int
        energy pos =
            let
                ( x, y, z ) =
                    pos
            in
            abs x + abs y + abs z

        potentialEnergies =
            positions |> List.map energy

        kineticEnergies =
            vels |> List.map energy
    in
    List.map2 (*) potentialEnergies kineticEnergies |> List.sum


repeat : (a -> a) -> Int -> a -> a
repeat f times input =
    if times <= 0 then
        input

    else
        repeat f (times - 1) (f input)


problemA : Planets -> Int
problemA planets =
    repeat step 1 planets |> totalEnergy


problemB : Planets -> Int
problemB planets =
    let
        rec curPlanets n seen =
            if seen |> Set.member curPlanets then
                n

            else
                rec (step curPlanets) (n + 1) (seen |> Set.insert curPlanets)
    in
    rec planets 0 Set.empty
