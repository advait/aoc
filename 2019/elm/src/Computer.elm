module Computer exposing (..)

import Array exposing (Array)
import Util


{-| Represents the state of the computer at any time.
-}
type alias Computer =
    { memory : Array Int
    , iPtr : Int
    , inputs : List InputOutput
    , output : List InputOutput
    }


{-| Type for a location in memory.
-}
type alias Loc =
    Int


{-| Type for the computer's input or output.
-}
type alias InputOutput =
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
            Maybe.map Output (immOrPosParamValue 1)

        Just 5 ->
            Maybe.map2 JumpIfTrue (immOrPosParamValue 1) (immOrPosParamValue 2)

        Just 6 ->
            Maybe.map2 JumpIfFalse (immOrPosParamValue 1) (immOrPosParamValue 2)

        Just 7 ->
            Maybe.map3 LessThan (immOrPosParamValue 1) (immOrPosParamValue 2) (immParamValue 3)

        Just 8 ->
            Maybe.map3 Equals (immOrPosParamValue 1) (immOrPosParamValue 2) (immParamValue 3)

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
    | JumpIfTrue Int Loc
    | JumpIfFalse Int Loc
    | LessThan Int Int Loc
    | Equals Int Int Loc
    | Halt


{-| Represents a low-level state modification to the computer.
-}
type StateT
    = Store Int Loc
    | AddIPtr Int
    | SetIPtr Loc
    | PopInputAndStore Loc
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
            [ PopInputAndStore dest, AddIPtr 2 ]

        Output dest ->
            [ AppendOutput dest, AddIPtr 2 ]

        JumpIfTrue p1 dest ->
            if p1 /= 0 then
                [ SetIPtr dest ]

            else
                [ AddIPtr 3 ]

        JumpIfFalse p1 dest ->
            if p1 == 0 then
                [ SetIPtr dest ]

            else
                [ AddIPtr 3 ]

        LessThan p1 p2 dest ->
            [ Store (p1 < p2 |> Util.boolToInt) dest, AddIPtr 4 ]

        Equals p1 p2 dest ->
            [ Store (p1 == p2 |> Util.boolToInt) dest, AddIPtr 4 ]

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

        SetIPtr dest ->
            { comp | iPtr = dest }

        PopInputAndStore dest ->
            case comp.inputs of
                [] ->
                    Debug.todo ("Error: Trying to read empty input: " ++ Debug.toString comp)

                head :: tail ->
                    { comp | memory = comp.memory |> Array.set dest head, inputs = tail }

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
    , inputs = []
    , output = []
    }


{-| Enqueue the given input to be consumed. Note that inputs will be consumed in the order
they were enqueued.
-}
enqueueInput : InputOutput -> Computer -> Computer
enqueueInput input comp =
    { comp | inputs = comp.inputs ++ [ input ] }


{-| Provides the given input to the computer.
-}
withInputs : List InputOutput -> Computer -> Computer
withInputs inputs comp =
    { comp | inputs = inputs }


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


{-| Keep on executing until the predicate returns True.
-}
execUntil : (Computer -> Bool) -> Computer -> Computer
execUntil pred comp =
    if pred comp then
        comp

    else
        case comp |> stepOnce of
            Nothing ->
                Debug.todo "Bad computer state"

            Just newComp ->
                execUntil pred newComp


{-| Keep stepping until we reach halt or an invalid state.
-}
execUntilHalt : Computer -> Computer
execUntilHalt =
    execUntil isHalted


{-| Keep stepping until the computer has outputted a new value.
-}
execUntilOutput : Computer -> Computer
execUntilOutput startingComp =
    let
        hasNewOutput curComp =
            startingComp.output /= curComp.output
    in
    execUntil hasNewOutput startingComp
