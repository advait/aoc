module ComputerViewer exposing (..)

import Array
import Browser
import Bulma.CDN exposing (..)
import Bulma.Columns exposing (..)
import Bulma.Components
import Bulma.Elements exposing (..)
import Bulma.Layout exposing (..)
import Bulma.Modifiers exposing (..)
import Computer exposing (Computer)
import Html exposing (Html, div, main_, text)
import Html.Events exposing (onClick)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { prevComps : List Computer
    , comp : Computer
    }


dummyInput =
    --"3,225,1,225,6,6,1100,1,238,225,104,0,1102,45,16,225,2,65,191,224,1001,224,-3172,224,4,224,102,8,223,223,1001,224,5,224,1,223,224,223,1102,90,55,225,101,77,143,224,101,-127,224,224,4,224,102,8,223,223,1001,224,7,224,1,223,224,223,1102,52,6,225,1101,65,90,225,1102,75,58,225,1102,53,17,224,1001,224,-901,224,4,224,1002,223,8,223,1001,224,3,224,1,224,223,223,1002,69,79,224,1001,224,-5135,224,4,224,1002,223,8,223,1001,224,5,224,1,224,223,223,102,48,40,224,1001,224,-2640,224,4,224,102,8,223,223,1001,224,1,224,1,224,223,223,1101,50,22,225,1001,218,29,224,101,-119,224,224,4,224,102,8,223,223,1001,224,2,224,1,223,224,223,1101,48,19,224,1001,224,-67,224,4,224,102,8,223,223,1001,224,6,224,1,223,224,223,1101,61,77,225,1,13,74,224,1001,224,-103,224,4,224,1002,223,8,223,101,3,224,224,1,224,223,223,1102,28,90,225,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,7,226,677,224,102,2,223,223,1005,224,329,1001,223,1,223,8,226,677,224,1002,223,2,223,1005,224,344,101,1,223,223,8,226,226,224,1002,223,2,223,1006,224,359,101,1,223,223,1008,677,226,224,1002,223,2,223,1005,224,374,1001,223,1,223,108,677,677,224,1002,223,2,223,1005,224,389,1001,223,1,223,1107,226,677,224,1002,223,2,223,1006,224,404,101,1,223,223,1008,226,226,224,102,2,223,223,1006,224,419,1001,223,1,223,7,677,226,224,1002,223,2,223,1005,224,434,101,1,223,223,1108,226,226,224,1002,223,2,223,1005,224,449,101,1,223,223,7,226,226,224,102,2,223,223,1005,224,464,101,1,223,223,108,677,226,224,102,2,223,223,1005,224,479,1001,223,1,223,1007,677,226,224,1002,223,2,223,1006,224,494,1001,223,1,223,1007,677,677,224,1002,223,2,223,1006,224,509,1001,223,1,223,107,677,677,224,1002,223,2,223,1005,224,524,101,1,223,223,1108,226,677,224,102,2,223,223,1006,224,539,1001,223,1,223,8,677,226,224,102,2,223,223,1005,224,554,101,1,223,223,1007,226,226,224,102,2,223,223,1006,224,569,1001,223,1,223,107,677,226,224,102,2,223,223,1005,224,584,1001,223,1,223,108,226,226,224,102,2,223,223,1006,224,599,1001,223,1,223,107,226,226,224,1002,223,2,223,1006,224,614,1001,223,1,223,1108,677,226,224,1002,223,2,223,1005,224,629,1001,223,1,223,1107,677,677,224,102,2,223,223,1005,224,644,1001,223,1,223,1008,677,677,224,102,2,223,223,1005,224,659,101,1,223,223,1107,677,226,224,1002,223,2,223,1006,224,674,101,1,223,223,4,223,99,226"
    "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"


init : Model
init =
    { prevComps = []
    , comp = Computer.fromString dummyInput |> Computer.withInput 1
    }



-- UPDATE


type Msg
    = StepForward
    | StepBackward


update : Msg -> Model -> Model
update msg model =
    case msg of
        StepForward ->
            case model.comp |> Computer.stepOnce of
                Nothing ->
                    model

                Just nextComp ->
                    { model | comp = nextComp, prevComps = model.comp :: model.prevComps }

        StepBackward ->
            case model.prevComps of
                head :: tail ->
                    { model | comp = head, prevComps = tail }

                _ ->
                    model



-- VIEW


view : Model -> Html Msg
view model =
    main_ []
        [ stylesheet
        , exampleHero
        , exampleColumns model.comp
        ]


exampleHero : Html Msg
exampleHero =
    hero { heroModifiers | size = Small, color = Primary }
        []
        [ heroBody []
            [ container []
                [ title H1 [] [ text "AOC: Intcode Emulator" ]
                , subtitle H3 [] [ text "Simulate" ]
                ]
            ]
        ]


exampleColumns : Computer -> Html Msg
exampleColumns comp =
    section NotSpaced
        []
        [ container []
            [ columns columnsModifiers
                []
                [ column columnModifiers [] (viewMemory comp)
                , column columnModifiers [] (viewInternals comp)
                , column columnModifiers [] (viewCommands comp)
                ]
            ]
        ]


viewMemory : Computer -> List (Html Msg)
viewMemory comp =
    let
        foo =
            comp.memory
                |> Array.indexedMap
                    (\loc value ->
                        tableRow (comp.iPtr == loc)
                            []
                            [ tableCell [] [ text (loc |> String.fromInt) ]
                            , tableCell [] [ text (value |> String.fromInt) ]
                            ]
                    )
                |> Array.toList
    in
    [ title H3 [] [ text "Memory" ]
    , table tableModifiers
        []
        [ tableHead []
            [ tableRow False [] [ tableCell [] [ text "Loc" ], tableCell [] [ text "Val" ] ] ]
        , tableBody [] foo
        ]
    ]


viewInternals : Computer -> List (Html Msg)
viewInternals comp =
    [ title H3 [] [ text "Internals" ]
    , title H4 [] [ text "Current Op" ]
    , box []
        [ text (comp |> Computer.readOp |> Debug.toString)
        ]
    , title H4 [] [ text "Transformations" ]
    , box []
        [ text (comp |> Computer.readOp |> Maybe.map Computer.execOp |> Debug.toString)
        ]
    , title H4 [] [ text "Input" ]
    , box []
        [ text (comp.input |> Debug.toString)
        ]
    , title H4 [] [ text "Outputs" ]
    , box []
        [ text (comp.output |> Debug.toString)
        ]
    ]


viewCommands : Computer -> List (Html Msg)
viewCommands comp =
    [ title H3 [] [ text "Commands" ]
    , box []
        [ button buttonModifiers [ onClick StepBackward ] [ text "Step Backward" ]
        , button buttonModifiers [ onClick StepForward ] [ text "Step Forward" ]
        ]
    ]
