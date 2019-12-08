module Problem7 exposing (..)

import Computer exposing (Computer, InputOutput)
import Util


{-| Executes a single pass through a single amplifier, pausing and returning the output.
-}
execSingleAmp : Computer -> InputOutput -> ( Computer, InputOutput )
execSingleAmp comp input =
    comp
        |> Computer.enqueueInput input
        |> Computer.execUntilOutput
        |> (\outComp -> ( outComp, outComp.output |> Util.unsafeHead ))


{-| Given an amp input, executes a single pass through the amps, returning the resulting amps
and an output value.
-}
execAmpsSinglePass : InputOutput -> List Computer -> ( List Computer, InputOutput )
execAmpsSinglePass input amps =
    Util.mapAndFoldl execSingleAmp input amps


{-| Repeatedly executes a list of amps in a feedback loop until they all halt.
-}
execAmpsUntilHalt : InputOutput -> List Computer -> ( List Computer, InputOutput )
execAmpsUntilHalt input amps =
    if amps |> List.all Computer.isHalted then
        ( amps, input )

    else
        let
            ( nextAmps, output ) =
                execAmpsSinglePass input amps
        in
        execAmpsUntilHalt output nextAmps


{-| An initial amp configuration input as distinct from the InputOutput of amps.
-}
type alias AmpConfig =
    Int


configsToAmps : List AmpConfig -> List Computer
configsToAmps configs =
    configs |> List.map (\value -> startingComputer |> Computer.enqueueInput value)


problemA =
    Util.permutations (List.range 0 4)
        |> List.map configsToAmps
        |> List.map (execAmpsSinglePass 0)
        |> List.map Tuple.second
        |> List.maximum


problemB =
    Util.permutations (List.range 5 9)
        |> List.map configsToAmps
        |> List.map (execAmpsUntilHalt 0)
        |> List.map Tuple.second
        |> List.maximum


puzzleInput =
    "3,8,1001,8,10,8,105,1,0,0,21,38,47,64,89,110,191,272,353,434,99999,3,9,101,4,9,9,102,3,9,9,101,5,9,9,4,9,99,3,9,1002,9,5,9,4,9,99,3,9,101,2,9,9,102,5,9,9,1001,9,5,9,4,9,99,3,9,1001,9,5,9,102,4,9,9,1001,9,5,9,1002,9,2,9,1001,9,3,9,4,9,99,3,9,102,2,9,9,101,4,9,9,1002,9,4,9,1001,9,4,9,4,9,99,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,99,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,99"


startingComputer =
    puzzleInput |> Computer.fromString
