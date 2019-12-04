module Problem2 exposing (..)

import List
import Util


type alias Computer =
    { memory : List Int
    , iPtr : Int
    }


{-| Creates a new computer with an iPtr of 0.
-}
newComputer : List Int -> Computer
newComputer mem =
    { memory = mem, iPtr = 0 }


{-| Returns a new computer with the given noun and verb replaced.
-}
withNewNounAndVerb : Int -> Int -> Computer -> Computer
withNewNounAndVerb noun verb comp =
    comp.memory |> listSetAt 1 noun |> listSetAt 2 verb |> newComputer


{-| Sets the ith element of the list ot the given value, returning the original list if it is too short.
-}
listSetAt : Int -> a -> List a -> List a
listSetAt indexToSet value list =
    let
        overwrite i originalValue =
            if i == indexToSet then
                value

            else
                originalValue
    in
    list |> List.indexedMap overwrite


{-| Returns the nth item of the list or crashes if it is too short.
-}
listGetAtWithDefault : a -> Int -> List a -> a
listGetAtWithDefault default i l =
    case l of
        [] ->
            default

        head :: tail ->
            if i == 0 then
                head

            else
                listGetAtWithDefault default (i - 1) tail


{-| Writes the given value to the given position in memory, advances the instruction pointer,
returning the new computer.
-}
writeValueAndAdvance : Int -> Int -> Computer -> Computer
writeValueAndAdvance pos val comp =
    let
        newMem =
            comp.memory |> listSetAt pos val

        newIPtr =
            comp.iPtr + 4
    in
    { memory = newMem, iPtr = newIPtr }


{-| Executes instructions, returning the state after halt.
-}
execUntilHalt : Computer -> Computer
execUntilHalt comp =
    let
        readMem loc =
            comp.memory |> listGetAtWithDefault 0 loc

        op =
            readMem comp.iPtr

        p1 =
            readMem (comp.iPtr + 1)

        p2 =
            readMem (comp.iPtr + 2)

        dest =
            readMem (comp.iPtr + 3)
    in
    case op of
        1 ->
            writeValueAndAdvance dest (readMem p1 + readMem p2) comp |> execUntilHalt

        2 ->
            writeValueAndAdvance dest (readMem p1 * readMem p2) comp |> execUntilHalt

        99 ->
            comp

        _ ->
            Debug.log "Invalid opcode: " comp


{-| Parses the program input, returning a computer.
-}
inputToComputer : String -> Computer
inputToComputer input =
    let
        memory =
            input |> String.split "," |> String.join "\n" |> Util.readInts
    in
    memory |> newComputer


{-| Solves Problem A.
-}
problemA : String -> Int
problemA input =
    input |> inputToComputer |> execUntilHalt |> .memory |> List.head |> Maybe.withDefault 0


{-| Solves Problem B.
-}
problemB : Int -> String -> Int
problemB desiredOutput input =
    let
        nounsAndVerbs =
            Util.permutations2 (List.range 0 99)

        execWithNounAndVerb noun verb =
            input |> inputToComputer |> withNewNounAndVerb noun verb |> execUntilHalt |> .memory |> List.head |> Maybe.withDefault 0

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
