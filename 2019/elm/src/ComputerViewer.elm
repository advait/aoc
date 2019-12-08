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
import Html exposing (Html, a, main_, text)
import Html.Attributes exposing (href, style)
import Html.Events exposing (onClick)


{-| MAIN
-}
main =
    Browser.sandbox { init = init, update = update, view = view }


{-| MODEL
-}
type alias Model =
    { prevComps : List Computer
    , comp : Computer
    }


dummyInput =
    "3,8,1001,8,10,8,105,1,0,0,21,38,47,64,89,110,191,272,353,434,99999,3,9,101,4,9,9,102,3,9,9,101,5,9,9,4,9,99,3,9,1002,9,5,9,4,9,99,3,9,101,2,9,9,102,5,9,9,1001,9,5,9,4,9,99,3,9,1001,9,5,9,102,4,9,9,1001,9,5,9,1002,9,2,9,1001,9,3,9,4,9,99,3,9,102,2,9,9,101,4,9,9,1002,9,4,9,1001,9,4,9,4,9,99,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,99,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,99"


init : Model
init =
    { prevComps = []
    , comp = Computer.fromString dummyInput |> Computer.withInputs [ 4, 68 ]
    }


{-| UPDATE
-}
type Msg
    = StepForward
    | StepBackward


update : Msg -> Model -> Model
update msg model =
    case msg of
        StepForward ->
            if model.comp |> Computer.isHalted then
                model

            else
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
        , viewHero
        , viewColumns model
        ]


viewHero : Html Msg
viewHero =
    hero { heroModifiers | size = Small, color = Primary }
        []
        [ heroBody []
            [ container []
                [ title H1 [] [ text "AOC: Intcode Emulator" ]
                , subtitle H3 [] [ text "Simulate the ", a [ href "https://adventofcode.com/2019/day/5" ] [ text "Advent of Code" ], text " intcode computer!" ]
                ]
            ]
        ]


viewColumns : Model -> Html Msg
viewColumns model =
    section NotSpaced
        []
        [ container []
            [ columns { columnsModifiers | gap = Gap8 }
                []
                [ column narrowColumnModifiers [] (viewCommands model)
                , column columnModifiers [] (viewInternals model.comp)
                , column narrowColumnModifiers [] (viewMemory model.comp)
                ]
            ]
        ]


viewMemory : Computer -> List (Html Msg)
viewMemory comp =
    let
        viewMemoryCell =
            comp.memory
                |> Array.indexedMap
                    (\loc value ->
                        tableRow (comp.iPtr == loc)
                            []
                            [ tableCell [ style "font-family" "monospace" ] [ text (loc |> String.fromInt) ]
                            , tableCell [ style "font-family" "monospace" ] [ text (value |> String.fromInt) ]
                            ]
                    )
                |> Array.toList
    in
    [ Bulma.Components.card []
        [ Bulma.Components.cardHeader []
            [ Bulma.Components.cardTitle []
                [ title H3 [] [ text "Memory" ]
                ]
            ]
        , Bulma.Components.cardContent []
            [ table { tableModifiers | bordered = True, striped = True, hoverable = True }
                [ style "width" "200px" ]
                [ tableHead []
                    [ tableRow False [] [ tableCellHead [] [ text "Loc" ], tableCellHead [] [ text "Val" ] ] ]
                , tableBody [] viewMemoryCell
                ]
            ]
        ]
    ]


viewInternals : Computer -> List (Html Msg)
viewInternals comp =
    [ Bulma.Components.card []
        [ Bulma.Components.cardHeader []
            [ Bulma.Components.cardTitle []
                [ title H3 [] [ text "Internals" ]
                ]
            ]
        , Bulma.Components.cardContent []
            [ title H4 [] [ text "Current Op" ]
            , box []
                [ text (comp |> Computer.readOp |> Debug.toString)
                ]
            , title H4 [] [ text "Transformations" ]
            , box []
                [ text (comp |> Computer.readOp |> Maybe.map Computer.execOp |> Debug.toString)
                ]
            , title H4 [] [ text "Input" ]
            , box []
                [ text (comp.inputs |> Debug.toString)
                ]
            , title H4 [] [ text "Outputs" ]
            , box []
                [ text (comp.output |> Debug.toString)
                ]
            ]
        ]
    ]


viewCommands : Model -> List (Html Msg)
viewCommands model =
    let
        backwardDisabled =
            model.prevComps == []

        forwardDisabled =
            model.comp |> Computer.isHalted
    in
    [ Bulma.Components.card []
        [ Bulma.Components.cardHeader []
            [ Bulma.Components.cardTitle []
                [ title H3 [] [ text "Commands" ]
                ]
            ]
        , Bulma.Components.cardContent []
            [ connectedButtons Centered
                []
                [ button { buttonModifiers | inverted = backwardDisabled, color = Warning } [ onClick StepBackward ] [ text "Step Backward" ]
                , button { buttonModifiers | inverted = forwardDisabled, color = Primary } [ onClick StepForward ] [ text "Step Forward" ]
                ]
            ]
        ]
    ]
