module Computer exposing (..)

import Array exposing (Array)
import Util


{-| Represents the state of the computer at any time.
-}
type alias Computer =
    { memory : Array Int
    , iPtr : Loc
    , basePtr : Loc
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


{-| Reads a memory location, returning zero if there is no value there.
-}
readMem : Computer -> Loc -> Int
readMem comp loc =
    comp.memory |> Array.get loc |> Maybe.withDefault 0


{-| Given the state of the computer, read the current Op, returning Nothing if the iPtr points off memory, or the
opcode is invalid.
-}
readOp : Computer -> Maybe LogicalOp
readOp comp =
    let
        -- Determines the parameter mode of the given zero-based index parameter
        paramMode : Int -> Maybe ParamMode
        paramMode paramIndex =
            readMem comp comp.iPtr
                |> (\opc -> opc // (10 ^ (paramIndex + 2)))
                |> (\i ->
                        case i |> modBy 10 of
                            0 ->
                                Just Position

                            1 ->
                                Just Immediate

                            2 ->
                                Just BaseRelative

                            _ ->
                                Nothing
                   )

        -- Given an argument index and a parameter type, return an Arg value representing that
        readArg : Int -> ParamType -> Maybe Arg
        readArg paramIndex paramType =
            paramMode paramIndex
                |> Maybe.map (\mode -> ( paramType, mode, readMem comp (comp.iPtr + 1 + paramIndex) ))

        parsedOp =
            opFromInt (readMem comp comp.iPtr)

        parsedOpName =
            parsedOp |> Maybe.map Tuple.first

        parsedArgs =
            parsedOp
                |> Maybe.map Tuple.second
                |> Maybe.map
                    (List.indexedMap readArg)
                |> Maybe.andThen Util.concatMaybesIfAllJusts
    in
    Maybe.map2 Tuple.pair parsedOpName parsedArgs


{-| Whether a parameter is an input Argument or an output Destination.
-}
type ParamType
    = Arg
    | Dest


{-| The parameter mode of an argument/destination. Position mode implies a memory location. Immediate mode implies
a constant/immediate value. BaseRelative mode implies a memory location relative to the base pointer.
-}
type ParamMode
    = Position
    | Immediate
    | BaseRelative


{-| Represents an argument that can be dereferenced based on its parameter mode.
-}
type alias Arg =
    ( ParamType, ParamMode, Int )


{-| The various operations that our computer supports.
-}
type OpName
    = AddOp
    | MulOp
    | InputOp
    | OutputOp
    | JumpIfTrueOp
    | JumpIfFalseOp
    | LessThanOp
    | EqualsOp
    | AdjustRelativeBaseOp
    | HaltOp


{-| Parses an integer, returning an OpName and a list of parameters that that op expects.
-}
opFromInt : Int -> Maybe ( OpName, List ParamType )
opFromInt instr =
    case instr |> modBy 100 of
        1 ->
            Just ( AddOp, [ Arg, Arg, Dest ] )

        2 ->
            Just ( MulOp, [ Arg, Arg, Dest ] )

        3 ->
            Just ( InputOp, [ Dest ] )

        4 ->
            Just ( OutputOp, [ Arg ] )

        5 ->
            Just ( JumpIfTrueOp, [ Arg, Arg ] )

        6 ->
            Just ( JumpIfFalseOp, [ Arg, Arg ] )

        7 ->
            Just ( LessThanOp, [ Arg, Arg, Dest ] )

        8 ->
            Just ( EqualsOp, [ Arg, Arg, Dest ] )

        9 ->
            Just ( AdjustRelativeBaseOp, [ Arg ] )

        99 ->
            Just ( HaltOp, [] )

        _ ->
            Nothing


{-| Represents a logical operation for the computer.
-}
type alias LogicalOp =
    ( OpName, List Arg )


{-| Represents a dereferenced operation for the computer, implying that position mode, immediate mode, or base
relative mode has been dereferenced to the provided list of Int values.
-}
type alias DereferencedOp =
    ( OpName, List Int )


{-| Given a logical operation, dereferences the arguments and the destination, providing constant values for execution.
-}
dereferenceLogicalOp : Computer -> LogicalOp -> Maybe DereferencedOp
dereferenceLogicalOp comp op =
    let
        ( opName, args ) =
            op

        -- Interprets position, immediate, and base relative mode, returning a dereferenced/resolved value
        dereference : Arg -> Maybe Int
        dereference arg =
            case arg of
                ( Arg, Position, value ) ->
                    Just <| readMem comp value

                ( Arg, Immediate, value ) ->
                    Just <| value

                ( Arg, BaseRelative, value ) ->
                    Just <| readMem comp (comp.basePtr + value)

                ( Dest, Position, value ) ->
                    Just <| value

                ( Dest, Immediate, _ ) ->
                    Debug.log "Error, destination should not be an immediate" Nothing

                ( Dest, BaseRelative, value ) ->
                    Just <| comp.basePtr + value

        dereferencedArgs =
            args |> List.map dereference |> Util.concatMaybesIfAllJusts
    in
    Maybe.map2 Tuple.pair (Just opName) dereferencedArgs


{-| Represents a low-level state modification to the computer.
-}
type StateT
    = Store Int Loc
    | AddIPtr Int
    | SetIPtr Loc
    | PopInputAndStore Loc
    | AppendOutput Int
    | AddBasePtr Int


{-| Transforms an operation into a sequence of computer state transformations.
-}
execOp : DereferencedOp -> Maybe (List StateT)
execOp op =
    case op of
        ( AddOp, [ p1, p2, dest ] ) ->
            Just <| [ Store (p1 + p2) dest, AddIPtr 4 ]

        ( MulOp, [ p1, p2, dest ] ) ->
            Just <| [ Store (p1 * p2) dest, AddIPtr 4 ]

        ( InputOp, [ dest ] ) ->
            Just <| [ PopInputAndStore dest, AddIPtr 2 ]

        ( OutputOp, [ p1 ] ) ->
            Just <| [ AppendOutput p1, AddIPtr 2 ]

        ( JumpIfTrueOp, [ p1, dest ] ) ->
            if p1 /= 0 then
                Just <| [ SetIPtr dest ]

            else
                Just <| [ AddIPtr 3 ]

        ( JumpIfFalseOp, [ p1, dest ] ) ->
            if p1 == 0 then
                Just <| [ SetIPtr dest ]

            else
                Just <| [ AddIPtr 3 ]

        ( LessThanOp, [ p1, p2, dest ] ) ->
            Just <| [ Store (p1 < p2 |> Util.boolToInt) dest, AddIPtr 4 ]

        ( EqualsOp, [ p1, p2, dest ] ) ->
            Just <| [ Store (p1 == p2 |> Util.boolToInt) dest, AddIPtr 4 ]

        ( AdjustRelativeBaseOp, [ p1 ] ) ->
            Just <| [ AddBasePtr p1, AddIPtr 2 ]

        ( HaltOp, [] ) ->
            Just <| []

        _ ->
            Debug.log "Illegal dereferenced operation" Nothing


{-| Performs the low-level state modification to the computer.
-}
execStateT : StateT -> Computer -> Computer
execStateT stateT comp =
    case stateT of
        Store val dest ->
            { comp | memory = comp.memory |> Util.safeArraySet dest val }

        AddIPtr jmp ->
            { comp | iPtr = comp.iPtr + jmp }

        SetIPtr dest ->
            { comp | iPtr = dest }

        PopInputAndStore dest ->
            case comp.inputs of
                [] ->
                    Debug.todo ("Error: Trying to read empty input: " ++ Debug.toString comp)

                head :: tail ->
                    { comp | memory = comp.memory |> Util.safeArraySet dest head, inputs = tail }

        AppendOutput val ->
            { comp | output = comp.output ++ [ val ] }

        AddBasePtr delta ->
            { comp | basePtr = comp.basePtr + delta }


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
    , basePtr = 0
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
    (comp |> readOp |> Maybe.map Tuple.first) == Just HaltOp


{-| View the internals of involved in executing a single instruction.
-}
stepInternals : Computer -> ( Maybe LogicalOp, Maybe DereferencedOp, Maybe (List StateT) )
stepInternals comp =
    let
        logicalOp =
            comp |> readOp

        dereferencedOp =
            logicalOp |> Maybe.andThen (dereferenceLogicalOp comp)

        stateTs =
            dereferencedOp |> Maybe.andThen execOp
    in
    ( logicalOp, dereferencedOp, stateTs )


{-| Executes a single instruction.
-}
stepOnce : Computer -> Maybe Computer
stepOnce comp =
    let
        ( _, _, stateTs ) =
            stepInternals comp
    in
    stateTs |> Maybe.map (List.foldl execStateT comp)


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
