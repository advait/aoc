module Problem2 exposing (..)

import Array exposing (Array)
import Computer
import List
import Util


type alias Computer =
    Computer.Comp


{-| Creates a new computer with an iPtr of 0.
-}
newComputer : Array Int -> Computer
newComputer =
    Computer.compWithMem


{-| Returns a new computer with the given noun and verb replaced.
-}
withNewNounAndVerb : Int -> Int -> Computer -> Computer
withNewNounAndVerb noun verb comp =
    comp.memory |> Array.set 1 noun |> Array.set 2 verb |> newComputer


{-| Executes instructions, returning the state after halt.
-}
execUntilHalt : Computer -> Computer
execUntilHalt comp =
    Computer.execUntilHalt comp |> Maybe.withDefault comp


{-| Parses the program input, returning a computer.
-}
inputToComputer : String -> Computer
inputToComputer input =
    let
        memory =
            input |> String.split "," |> String.join "\n" |> Util.readInts |> Array.fromList
    in
    memory |> newComputer


{-| Solves Problem A.
-}
problemA : String -> Int
problemA input =
    input |> inputToComputer |> execUntilHalt |> .memory |> Array.get 0 |> Maybe.withDefault 0


{-| Solves Problem B.
-}
problemB : Int -> String -> Int
problemB desiredOutput input =
    let
        nounsAndVerbs =
            Util.permutations2 (List.range 0 99)

        execWithNounAndVerb noun verb =
            input |> inputToComputer |> withNewNounAndVerb noun verb |> execUntilHalt |> .memory |> Array.get 0 |> Maybe.withDefault 0

        rec : List ( Int, Int ) -> Int
        rec remainingNounsAndVerbs =
            case remainingNounsAndVerbs of
                [] ->
                    Debug.todo "Tried all pairs of nouns and verbs and failed"

                ( noun, verb ) :: tail ->
                    if execWithNounAndVerb noun verb == desiredOutput then
                        100 * noun + verb

                    else
                        rec tail
    in
    rec nounsAndVerbs
