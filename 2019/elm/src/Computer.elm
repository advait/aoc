module Computer exposing (..)

import Array exposing (Array)
import Util


{-| Represents the state of the computer at any time.
-}
type alias Computer =
    { memory : Array Int
    , iPtr : Int
    , input : Int
    , output : List Int
    }


{-| Type for a location in memory.
-}
type alias Loc =
    Int


{-| Given the state of the computer, read the current Op, returning Nothing if the iPtr points off memory, or the
opcode is invalid.
-}
readOp : Computer -> Maybe LogicalOp
readOp comp =
    let
        readMem : Loc -> Maybe Int
        readMem relativePos =
            comp.memory |> Array.get relativePos

        fullOpcode =
            readMem comp.iPtr

        -- Bottom two digits of full opcode
        opcode =
            fullOpcode |> Maybe.map (modBy 100)

        -- Determine if the given param (one-based index) is in immediate mode
        isImmediateMode : Int -> Maybe Bool
        isImmediateMode paramIndex =
            fullOpcode
                |> Maybe.map (\opc -> opc // (10 ^ (paramIndex + 1)))
                |> Maybe.map (\i -> (i |> modBy 10) == 1)

        -- Interprets the parameter as an immediate and returns it
        immParamValue : Int -> Maybe Int
        immParamValue index =
            readMem (comp.iPtr + index)

        -- Determines the de-referenced value for the given parameter
        immOrPosParamValue : Int -> Maybe Int
        immOrPosParamValue index =
            isImmediateMode index
                |> Maybe.andThen
                    (\isImmediate ->
                        if isImmediate then
                            immParamValue index

                        else
                            immParamValue index |> Maybe.andThen (\loc -> readMem loc)
                    )
    in
    case opcode of
        Just 1 ->
            Maybe.map3 Add (immOrPosParamValue 1) (immOrPosParamValue 2) (immParamValue 3)

        Just 2 ->
            Maybe.map3 Mul (immOrPosParamValue 1) (immOrPosParamValue 2) (immParamValue 3)

        Just 3 ->
            Maybe.map Input (immParamValue 1)

        Just 4 ->
            Maybe.map Output (immParamValue 1)

        Just 99 ->
            Just Halt

        _ ->
            Nothing


{-| Represents a logical operation for the computer. Does not care about immediate or position mode and expects any such
dereferencing to be handled beforehand.
-}
type LogicalOp
    = Add Int Int Loc
    | Mul Int Int Loc
    | Input Loc
    | Output Loc
    | Halt


{-| Represents a low-level state modification to the computer.
-}
type StateT
    = Store Int Loc
    | AddIPtr Int
    | StoreInputInto Loc
    | AppendOutput Int


{-| Transforms an operation into a sequence of computer state transformations.
-}
execOp : LogicalOp -> List StateT
execOp op =
    case op of
        Add p1 p2 dest ->
            [ Store (p1 + p2) dest, AddIPtr 4 ]

        Mul p1 p2 dest ->
            [ Store (p1 * p2) dest, AddIPtr 4 ]

        Input dest ->
            [ StoreInputInto dest, AddIPtr 2 ]

        Output dest ->
            [ AppendOutput dest, AddIPtr 2 ]

        Halt ->
            []


{-| Performs the low-level state modification to the computer.
-}
execStateT : StateT -> Computer -> Computer
execStateT stateT comp =
    case stateT of
        Store val dest ->
            { comp | memory = comp.memory |> Array.set dest val }

        AddIPtr jmp ->
            { comp | iPtr = comp.iPtr + jmp }

        StoreInputInto dest ->
            { comp | memory = comp.memory |> Array.set dest comp.input }

        AppendOutput val ->
            { comp | output = val :: comp.output }


{-| Parses the program input, returning a computer.
-}
fromString : String -> Computer
fromString input =
    let
        memory =
            input |> String.split "," |> String.join "\n" |> Util.readInts |> Array.fromList
    in
    withMem memory


{-| Instantiates a computer with the given memory.
-}
withMem : Array Int -> Computer
withMem mem =
    { memory = mem
    , iPtr = 0
    , input = 0
    , output = []
    }


{-| Provides the given input to the computer.
-}
withInput : Int -> Computer -> Computer
withInput input comp =
    { comp | input = input }


{-| Returns whether the computer is in a halted state.
-}
isHalted : Computer -> Bool
isHalted comp =
    (comp |> readOp) == Just Halt


{-| Executes a single instruction.
-}
stepOnce : Computer -> Maybe Computer
stepOnce comp =
    let
        op =
            comp |> readOp

        stateTs =
            op |> Maybe.map execOp

        finalComp =
            stateTs |> Maybe.map (List.foldl execStateT comp)
    in
    finalComp


{-| Keep stepping until we reach halt or an invalid state.
-}
execUntilHalt : Computer -> Computer
execUntilHalt comp =
    if (comp |> readOp) == Just Halt then
        comp

    else
        case comp |> stepOnce of
            Nothing ->
                comp

            Just newComp ->
                execUntilHalt newComp
