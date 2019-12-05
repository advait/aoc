module Computer exposing (..)

import Array exposing (Array)
import Bitwise
import Debug
import Util


{-| Represents the state of the computer at any time.
-}
type alias Comp =
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
readOp : Comp -> Maybe LogicalOp
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


{-| Represents an abstract state modification to the computer.
-}
type StateT
    = Store Int Loc
    | HaltT
    | AddIPtr Int
    | StoreInputInto Loc
    | AppendOutput Int


execOp : LogicalOp -> List StateT
execOp op =
    case op of
        Add p1 p2 dest ->
            [ Store (p1 + p2) dest, AddIPtr 4 ]

        Mul p1 p2 dest ->
            [ Store (p1 * p2) dest, AddIPtr 4 ]

        -- TODO (Advait): Consider removing HaltT and making implicit as empty List StateT
        Halt ->
            [ HaltT ]

        Input dest ->
            [ StoreInputInto dest, AddIPtr 2 ]

        Output dest ->
            [ AppendOutput dest, AddIPtr 2 ]


execStateT : StateT -> Comp -> Comp
execStateT stateT comp =
    case stateT of
        Store val dest ->
            { comp | memory = comp.memory |> Array.set dest val }

        HaltT ->
            comp

        AddIPtr jmp ->
            { comp | iPtr = comp.iPtr + jmp }

        StoreInputInto dest ->
            { comp | memory = comp.memory |> Array.set dest comp.input }

        AppendOutput val ->
            { comp | output = val :: comp.output }


{-| Parses the program input, returning a computer.
-}
inputToComputer : String -> Comp
inputToComputer input =
    let
        memory =
            input |> String.split "," |> String.join "\n" |> Util.readInts |> Array.fromList
    in
    compWithMem memory


{-| Instantiates a computer with the given memory.
-}
compWithMem : Array Int -> Comp
compWithMem mem =
    { memory = mem
    , iPtr = 0
    , input = 0
    , output = []
    }


isHalt : Comp -> Bool
isHalt comp =
    (comp |> readOp) == Just Halt


{-| Executes a single instruction.
-}
stepOnce : Comp -> Maybe Comp
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
execUntilHalt : Comp -> Maybe Comp
execUntilHalt comp =
    if (comp |> readOp) == Just Halt then
        Just comp

    else
        comp |> stepOnce |> Maybe.andThen execUntilHalt
