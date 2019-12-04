module Problem2 exposing (..)

import List
import Util


type alias Computer =
    { memory : List Int
    , iPtr : Int
    }


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
    if indexToSet > (list |> List.length) then
        Debug.log ("Setting invalid memory location " ++ String.fromInt indexToSet) list

    else
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


{-| Executes an instruction, returning the state after execution.
-}
exec : Computer -> Computer
exec comp =
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
            writeValueAndAdvance dest (readMem p1 + readMem p2) comp |> exec

        2 ->
            writeValueAndAdvance dest (readMem p1 * readMem p2) comp |> exec

        99 ->
            comp

        _ ->
            Debug.log "Invalid opcode: " comp


{-| Keeps executing until the head is Opcode 99.
-}
execUntilHalt : Computer -> Computer
execUntilHalt comp =
    case comp.memory |> List.head of
        Just 99 ->
            comp

        _ ->
            exec comp


inputToComputer : String -> Computer
inputToComputer input =
    let
        memory =
            input |> String.split "," |> String.join "\n" |> Util.readInts
    in
    memory |> (\mem -> { memory = mem, iPtr = 0 })


problemA : String -> List Int
problemA input =
    input |> inputToComputer |> execUntilHalt |> .memory
