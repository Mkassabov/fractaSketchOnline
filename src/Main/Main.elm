module Main exposing (..)

import Browser
import Browser.Dom
import Browser.Events
import ColorPicker
import Controls exposing (..)
import Draw
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Html
import Html.Attributes
import Html.Events as Events
import Json.Decode as Decode exposing (..)
import Model exposing (..)
import Msg exposing (..)
import Process exposing (..)
import Styles exposing (..)
import Task
import View


main : Program { x : Int, y : Int } Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : { x : Int, y : Int } -> ( Model, Cmd Msg )
init { x, y } =
    ( Model.init ( x, y ), Task.attempt (EventsMsg << GotElement) (Browser.Dom.getElement "canvas") )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize (\_ _ -> EventsMsg Resize)
        , Browser.Events.onKeyDown (Decode.map (EventsMsg << KeyPress) keyboardDecoder)
        ]


view : Model -> Html.Html Msg
view model =
    let
        mobile =
            isMobile model.windowSize

        tablet =
            isTablet model.windowSize
    in
    Element.layout
        [ Font.size (1 |> rem)
        , Font.color white
        , Font.center
        , Background.color background
        , clipY
        , htmlAttribute (Events.on "touchend" (Decode.map (EventsMsg << TouchEnd) touchDecoder))
        , htmlAttribute (Events.on "mouseup" (Decode.map (EventsMsg << MouseUp) (Decode.map (\x -> ( x, -1 )) mouseDecoder)))

        --
        ]
    <|
        Element.column
            [ centerX
            , padding
                (if mobile then
                    0.5 |> rem

                 else
                    2 |> rem
                )
            , spacing
                (if mobile then
                    0.5 |> rem

                 else
                    2 |> rem
                )
            , width fill
            , height fill
            ]
            [ Element.row [ centerX, spacing (2 |> rem), width fill ]
                [ if mobile || tablet || model.state == View then
                    Element.none

                  else
                    Element.el [ width fill ] Element.none
                , Element.el [ centerX ]
                    (Element.row
                        (containerCSS
                            [ centerX
                            , padding
                                ((if tablet then
                                    0

                                  else
                                    0.5
                                 )
                                    |> rem
                                )
                            ]
                        )
                        (generateMenu model.menu model)
                    )
                , if mobile || tablet || model.state == View then
                    Element.none

                  else
                    Element.el [ width fill ]
                        (Element.row (containerCSS [ alignLeft, padding (0.5 |> rem) ])
                            [ Element.column [ Font.color black, padding (0.5 |> rem), spacing (1 |> rem) ]
                                [ Element.text ("Template Lines: " ++ String.fromInt (max ((model.fractalModel.fractaldata.seg |> List.length) - 1) 0))
                                , Element.row []
                                    [ Element.text "Level: ", Element.el [] (Element.text (String.fromInt model.drawingModel.level)) ]
                                , Element.row []
                                    [ Element.text "Fractal Lines: ", Element.el [] (Element.text (String.fromInt (countSegments model.fractalModel.fractaldata model.drawingModel.level |> (\( p, q ) -> p + q)))) ]
                                ]
                            ]
                        )
                ]
            , Element.row (containerCSS [ width fill, height fill, htmlAttribute (Html.Attributes.id "canvas") ])
                [ html
                    (case model.state of
                        Iterating ->
                            View.view model

                        LiveDraw ->
                            View.liveView model

                        _ ->
                            Draw.view model
                    )
                ]
            ]


generateMenu : Menu -> Model -> List (Element Msg)
generateMenu menu model =
    case menu of
        Draw ->
            let
                mobile =
                    isMobile model.windowSize

                tablet =
                    isTablet model.windowSize

                fileControl =
                    Controls.generateGenericControls mobile
                        [ ( "Import", MenuMsg ImportMenu )
                        , ( "Finish", FractalMsg FinishDrawing )
                        , ( "Export", MenuMsg ExportMenu )
                        ]

                lineControl =
                    Controls.generateLineControl model (ControlsMsg << LineTypeMsg) model.fractalModel.lineType
            in
            if mobile then
                [ Element.column (subContainerCSS [ width fill ])
                    [ lineControl
                    , fileControl
                    ]
                ]

            else
                [ if tablet then
                    Element.none

                  else
                    Controls.generateGenericToggleList enumGridType (ControlsMsg << GridTypeMsg) model.drawingModel.grid
                , Element.column (subContainerCSS [ width (fillPortion 2), paddingEach { edges | bottom = 0 } ])
                    [ lineControl
                    , Element.el
                        [ width fill
                        , Element.below
                            (if model.showColorPicker then
                                Element.el [ width shrink, alignRight ]
                                    (html
                                        (ColorPicker.view (convertColorTypeBack model.fractalModel.color) model.colorPicker |> Html.map ColorPickerMsg)
                                    )

                             else
                                Element.none
                            )
                        ]
                        (Element.row [ width fill ]
                            [ Controls.generateSlider "line weight" 0.75 0.1 (\n -> ControlsMsg (SetLineWeight n)) 0.01 model.fractalModel.lineWeight
                            , Element.el
                                [ width shrink ]
                                (generateGenericToggle model.fractalModel.fractaldata.propagateLineWeight "Propagate" (ControlsMsg << SetPropagateLineWeight))
                            , Element.el
                                [ paddingEach { noEdges | left = 0.5 |> rem } ]
                                (Controls.generateColorControl model model.fractalModel.color)
                            ]
                        )
                    ]
                , fileControl
                ]

        Import ->
            [ Controls.generateGenericControls True
                [ ( "Model", FractalMsg ReturnToDrawing )
                , ( ".fsk", FileMsg ImportFsk )
                , ( ".fsk2", FileMsg ImportFsk2 )
                ]
            ]

        Export ->
            [ Controls.generateGenericControls True
                [ ( "Model", FractalMsg ReturnToDrawing )
                , ( ".fsk", FileMsg ExportFsk )
                , ( ".fsk2", FileMsg ExportFsk2 )
                , ( ".svg", FileMsg DownloadSVG )
                ]
            ]

        Iterate ->
            let
                mobile =
                    isMobile model.windowSize

                mainControl =
                    Controls.generateGenericControls True
                        [ ( "Model", FractalMsg ReturnToDrawing )
                        , ( "Prev", FractalMsg PreviousIteration )
                        , ( "Next", FractalMsg NextIteration )
                        , ( "Download", FileMsg DownloadSVG )
                        ]
            in
            if mobile then
                [ mainControl
                ]

            else
                [ Element.column (subContainerCSS [ paddingEach { edges | bottom = 0 } ])
                    [ mainControl
                    , Element.row [ width fill ]
                        [ Controls.generateSlider "accuracy" -0.1 -10 (\n -> ControlsMsg (SetAccuracy (-1 * n))) 0.1 (-1 * model.drawingModel.accuracy)
                        ]
                    ]
                , Controls.generateGenericToggleList enumGhostType (ControlsMsg << GhostTypeMsg) model.drawingModel.ghost
                ]
