port module Msg exposing (..)

import Browser.Dom
import ColorPicker exposing (..)
import Csv
import Element exposing (..)
import File exposing (File, toString)
import File.Download as Download
import File.Select as Select
import Fractal exposing (FractalData, addPoint, emptyFractalData, getPoint, movePoint, moveTemplate, removePoint, scaleTemplate, rotateTemplate, splitLine)
import Grid exposing (..)
import Html.Events as Events
import Json.Decode as Decode exposing (..)
import Line exposing (..)
import Model exposing (..)
import Point exposing (..)
import Process exposing (..)
import Result.Extra
import Svg exposing (..)
import Task


port downloadSVG : () -> Cmd msg


type Msg
    = Update Model
    | None
    | ControlsMsg ControlsMsgType
    | EventsMsg EventsMsgType
    | FractalMsg FractalMsgType
    | MenuMsg MenuMsgType
    | FileMsg FileMsgType
    | ColorPickerMsg ColorPicker.Msg
    | ShowColorPicker Bool


type ControlsMsgType
    = GhostTypeMsg Ghost
    | GridTypeMsg GridType
    | LineTypeMsg LineType
    | ScaleTemplate Float
    | RotateTemplate Float
    | SetAccuracy Float
    | SetLineWeight Float
    | SetPropagateLineWeight Bool
    | MoveModel ( Float, Float )


type EventsMsgType
    = GotElement (Result Browser.Dom.Error Browser.Dom.Element)
    | Resize
    | MouseClick MousePos
    | MouseDown ( MousePos, Int )
    | MouseMove MousePos
    | MouseUp ( MousePos, Int )
    | MouseLeave MousePos
    | SegClick ( MousePos, Int )
    | SegDblClick ( MousePos, Int )
    | RightClick ( MousePos, Int )
    | TouchStart ( Touches, Int )
    | TouchMove Touches
    | TouchEnd Touches
    | KeyPress KeyIn


type FractalMsgType
    = FinishDrawing
    | ReturnToDrawing
    | NextIteration
    | PreviousIteration
    | InfiniteIteration
    | InfiniteIterationSlow
    | PauseIteration


type MenuMsgType
    = DrawMenu
    | ImportMenu
    | ExportMenu
    | IterateMenu


type FileMsgType
    = DownloadSVG
    | ExportFsk
    | ExportFsk2
    | ImportFsk
    | ImportFsk2
    | FileFskToFractalData ( Bool, File )
    | SetFractalData Fractal.FractalData


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    -- case Debug.log "model" model of
    case model of
        _ ->
            -- case Debug.log "msg" message of
                case message of
                Update new ->
                    ( new, Cmd.none )

                None ->
                    ( model, Cmd.none )

                ControlsMsg msg ->
                    updateControlsMsg msg model

                EventsMsg msg ->
                    updateEventsMsg msg model

                FractalMsg msg ->
                    updateFractalMsg msg model

                MenuMsg msg ->
                    updateMenuMsg msg model

                FileMsg msg ->
                    updateFileMsg msg model

                ColorPickerMsg msg ->
                    ( updateColorPicker msg model, Cmd.none )

                ShowColorPicker bool ->
                    ( { model | showColorPicker = bool }, Cmd.none )


updateControlsMsg : ControlsMsgType -> Model -> ( Model, Cmd Msg )
updateControlsMsg msg model =
    case msg of
        GhostTypeMsg gt ->
            ( { model | drawingModel = model.drawingModel |> setGhost gt }, Cmd.none )

        GridTypeMsg gt ->
            if model.state == Drawing || model.state == LiveDraw then
                ( { model | drawingModel = model.drawingModel |> setGrid gt }, Cmd.none )

            else
                ( model, Cmd.none )

        LineTypeMsg ln ->
            if model.state == Drawing || model.state == LiveDraw then
                ( { model | fractalModel = model.fractalModel |> setLineType ln }, Cmd.none )

            else
                ( model, Cmd.none )

        ScaleTemplate n ->
            if model.state == Drawing || model.state == LiveDraw then
                ( { model
                    | fractalModel =
                        model.fractalModel
                            |> setFractalData (scaleTemplate model.fractalModel.fractaldata n)
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        RotateTemplate n ->
            if model.state == Drawing || model.state == LiveDraw then
                ( { model
                    | fractalModel =
                        model.fractalModel
                            |> setFractalData (rotateTemplate model.fractalModel.fractaldata n)
                  }
                , Cmd.none
                )
            else
                ( model, Cmd.none )

        SetAccuracy n ->
            ( { model
                | drawingModel =
                    model.drawingModel
                        |> setLevel 0
                        |> setAccuracy (max 0.1 n)
              }
            , Cmd.none
            )

        SetLineWeight n ->
            if model.state == Drawing || model.state == LiveDraw then
                ( { model | fractalModel = model.fractalModel |> setLineWeight n }, Cmd.none )

            else
                ( model, Cmd.none )

        SetPropagateLineWeight b ->
            if model.state == Drawing || model.state == LiveDraw then
                ( { model | fractalModel = model.fractalModel |> setPropagateLineWeight b }, Cmd.none )

            else
                ( model, Cmd.none )

        MoveModel offset ->
            if model.state == Drawing || model.state == LiveDraw then
                ( { model
                    | fractalModel =
                        model.fractalModel
                            |> setFractalData (moveTemplate model.fractalModel.fractaldata offset)
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )


updateEventsMsg : EventsMsgType -> Model -> ( Model, Cmd Msg )
updateEventsMsg msg model =
    case msg of
        GotElement v ->
            -- case Debug.log "element" v of
            case v of
                Ok w ->
                    ( { model
                        | canvasSize =
                            ( round (handleOverflow w.scene.width w.viewport.width w.element.width) - 6
                            , round (handleOverflow w.scene.height w.viewport.height w.element.height) - 6
                            )
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, sleep 100 |> Task.perform (\_ -> EventsMsg Resize) )

        Resize ->
            ( model, Task.attempt (EventsMsg << GotElement) (Browser.Dom.getElement "canvas") )

        MouseClick mp ->
            if model.state == Iterating || model.drawingModel.dragged /= Nothing then
                ( { model | drawingModel = model.drawingModel |> setDragged Nothing }, Cmd.none )

            else
                (\( i, j ) ->
                    ( { model
                        | fractalModel =
                            model.fractalModel
                                |> setFractalData (addPoint model.fractalModel.fractaldata (getLineData model.fractalModel) ( i, j ))
                        , drawingModel =
                            model.drawingModel
                                |> setDragged Nothing
                      }
                    , Cmd.none
                    )
                )
                    (gridSnap model.drawingModel.grid ( mp.layerX, mp.layerY ))

        MouseDown ( mp, i ) ->
            if mp.which == 1 then
                ( { model
                    | drawingModel =
                        model.drawingModel
                            |> setDragged (Just i)
                            |> setDraggedCoord ( mp.layerX, mp.layerY )
                            |> setExtraPoint Nothing
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        MouseMove mp ->
            case model.drawingModel.dragged of
                Nothing ->
                    if model.drawingModel.drawExtraPoint then
                        ( { model
                            | drawingModel =
                                model.drawingModel
                                    |> setExtraPoint (Just ( gridSnap model.drawingModel.grid ( mp.layerX, mp.layerY ), getLineData model.fractalModel ))
                          }
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

                Just i ->
                    ( { model
                        | fractalModel =
                            model.fractalModel
                                |> setFractalData (movePoint model.fractalModel.fractaldata i (gridSnap model.drawingModel.grid ( mp.layerX, mp.layerY )))
                      }
                    , Cmd.none
                    )

        MouseUp ( mp, i ) ->
            if (diffPoint model.drawingModel.draggedCoord ( mp.layerX, mp.layerY ) |> lenPoint) < 12 || i == -1 then
                ( { model | drawingModel = model.drawingModel |> setDragged Nothing }, Cmd.none )

            else
                ( model, Cmd.none )

        MouseLeave mp ->
            ( { model
                | drawingModel =
                    model.drawingModel
                        |> setExtraPoint Nothing
                        |> setDragged
                            (if mp.which == 1 then
                                model.drawingModel.dragged

                             else
                                Nothing
                            )
              }
            , Cmd.none
            )

        SegClick ( mp, i ) ->
            ( { model
                | fractalModel =
                    model.fractalModel
                        |> setFractalData (model.fractalModel.fractaldata |> Fractal.fracRotateSeg i)
              }
            , Cmd.none
            )

        SegDblClick ( mp, i ) ->
            ( { model | fractalModel = model.fractalModel |> setFractalData (splitLine model.fractalModel.fractaldata i) }
            , Cmd.none
            )

        TouchStart ( t, i ) ->
            case List.head t.targetTouches of
                Nothing ->
                    ( model, Cmd.none )

                Just ft ->
                    ( { model
                        | drawingModel =
                            model.drawingModel
                                |> setDragged (Just i)
                                |> setDraggedCoord (diffPoint (getPoint model.fractalModel.fractaldata i) ( ft.pagePosX, ft.pagePosY ))
                                |> setExtraPoint Nothing
                      }
                    , Cmd.none
                    )

        TouchMove t ->
            case List.head t.targetTouches of
                Nothing ->
                    ( model, Cmd.none )

                Just ft ->
                    case model.drawingModel.dragged of
                        Nothing ->
                            if model.drawingModel.drawExtraPoint then
                                ( { model
                                    | drawingModel =
                                        model.drawingModel
                                            |> setExtraPoint
                                                (Just
                                                    ( gridSnap model.drawingModel.grid
                                                        (diffPoint model.drawingModel.draggedCoord ( ft.pagePosX, ft.pagePosY ))
                                                    , getLineData model.fractalModel
                                                    )
                                                )
                                  }
                                , Cmd.none
                                )

                            else
                                ( model, Cmd.none )

                        Just i ->
                            ( { model
                                | fractalModel =
                                    model.fractalModel
                                        |> setFractalData
                                            (movePoint model.fractalModel.fractaldata
                                                i
                                                (gridSnap model.drawingModel.grid (diffPoint model.drawingModel.draggedCoord ( ft.pagePosX, ft.pagePosY )))
                                            )
                              }
                            , Cmd.none
                            )

        TouchEnd t ->
            ( { model
                | drawingModel =
                    model.drawingModel
                        |> setDragged Nothing
                        |> setDraggedCoord ( 0, 0 )
              }
            , Cmd.none
            )

        RightClick ( mp, i ) ->
            ( { model
                | fractalModel =
                    model.fractalModel
                        |> setFractalData (removePoint model.fractalModel.fractaldata i)
              }
            , Cmd.none
            )

        KeyPress k ->
            case k.key of
                "Control" ->
                    if model.state == Drawing || model.state == LiveDraw then
                        ( { model
                            | drawingModel =
                                model.drawingModel
                                    |> setDrawExtraPoint (not model.drawingModel.drawExtraPoint)
                                    |> setExtraPoint Nothing
                          }
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

                "0" ->
                    updateControlsMsg (LineTypeMsg Hidden) model

                "1" ->
                    if model.state == Drawing || model.state == LiveDraw then
                        updateControlsMsg (LineTypeMsg TopRight) model

                    else
                        updateControlsMsg (GhostTypeMsg NoGhost) model

                "2" ->
                    if model.state == Drawing || model.state == LiveDraw then
                        updateControlsMsg (LineTypeMsg TopLeft) model

                    else
                        updateControlsMsg (GhostTypeMsg LastGhost) model

                "3" ->
                    if model.state == Drawing || model.state == LiveDraw then
                        updateControlsMsg (LineTypeMsg BottomRight) model

                    else
                        updateControlsMsg (GhostTypeMsg AllGhost) model

                "4" ->
                    updateControlsMsg (LineTypeMsg BottomLeft) model

                "5" ->
                    updateControlsMsg (LineTypeMsg Static) model

                "n" ->
                    updateControlsMsg (GridTypeMsg Grid.None) model

                "s" ->
                    updateControlsMsg (GridTypeMsg Square) model

                "h" ->
                    updateControlsMsg (GridTypeMsg Hex) model

                "Enter" ->
                    updateFractalMsg FinishDrawing model

                "Escape" ->
                    updateFractalMsg ReturnToDrawing model

                "Backspace" ->
                    if model.state == Drawing || model.state == LiveDraw then
                        ( { model | fractalModel = model.fractalModel |> setFractalData emptyFractalData }, Cmd.none )

                    else
                        ( model, Cmd.none )

                ">" ->
                    updateControlsMsg (ScaleTemplate 1.25) model

                "<" ->
                    updateControlsMsg (ScaleTemplate 0.8) model

                "." ->
                    updateControlsMsg (RotateTemplate 5) model

                "," ->
                    updateControlsMsg (RotateTemplate -5) model

                "ArrowUp" ->
                    if model.state == Drawing || model.state == LiveDraw then
                        updateControlsMsg (MoveModel ( 0, -25 )) model

                    else
                        updateFractalMsg InfiniteIterationSlow model

                "ArrowRight" ->
                    if model.state == Drawing || model.state == LiveDraw then
                        updateControlsMsg (MoveModel ( 25, 0 )) model

                    else
                        updateFractalMsg NextIteration model

                "ArrowLeft" ->
                    if model.state == Drawing || model.state == LiveDraw then
                        updateControlsMsg (MoveModel ( -25, 0 )) model

                    else
                        updateFractalMsg PreviousIteration model

                "ArrowDown" ->
                    if model.state == Drawing || model.state == LiveDraw then
                        updateControlsMsg (MoveModel ( 0, 25 )) model

                    else
                        updateFractalMsg InfiniteIteration model

                " " ->
                    updateFractalMsg PauseIteration model

                "@" ->
                    if model.state == Drawing then
                        ( { model | state = LiveDraw }, Cmd.none )

                    else if model.state == LiveDraw then
                        ( { model | state = Drawing }, Cmd.none )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )


updateFractalMsg : FractalMsgType -> Model -> ( Model, Cmd Msg )
updateFractalMsg msg model =
    -- case Debug.log "msg" msg of
    case msg of
        FinishDrawing ->
            if model.state == Iterating then
                ( model, Cmd.none )

            else
                updateMenuMsg IterateMenu { model | state = Iterating, drawingModel = model.drawingModel |> setLevel 0 }

        ReturnToDrawing ->
            updateMenuMsg DrawMenu { model | state = Drawing, drawingModel = model.drawingModel |> setLevel 0 }

        NextIteration ->
            updateFractalMsg PauseIteration { model | drawingModel = model.drawingModel |> setLevel (model.drawingModel.level + 1) }

        PreviousIteration ->
            updateFractalMsg PauseIteration { model | drawingModel = model.drawingModel |> setLevel (max 0 (model.drawingModel.level - 1)) }

        InfiniteIteration ->
            if model.fractalModel.fractaldata |> Fractal.allowInfiniteIteration then
                updateFractalMsg PauseIteration { model | drawingModel = model.drawingModel |> setLevel 30 }

            else
                ( model, Cmd.none )

        InfiniteIterationSlow ->
            if model.drawingModel.level < 30 && model.state == Iterating && not model.drawingModel.pauseIteration then
                ( { model | drawingModel = model.drawingModel |> setLevel (model.drawingModel.level + 1) }, Cmd.none ) |> (\( m, _ ) -> ( m, Process.sleep 1000 |> Task.perform (\_ -> FractalMsg InfiniteIterationSlow) ))

            else
                ( model, Cmd.none )

        PauseIteration ->
            if model.drawingModel.pauseIteration then
                ( { model | drawingModel = model.drawingModel |> setPauseIteration False }, Cmd.none )

            else
                ( { model | drawingModel = model.drawingModel |> setPauseIteration True }
                , Process.sleep 1100 |> Task.perform (\_ -> FractalMsg PauseIteration)
                )


updateMenuMsg : MenuMsgType -> Model -> ( Model, Cmd Msg )
updateMenuMsg msg model =
    case msg of
        DrawMenu ->
            ( { model | menu = Draw }, Cmd.none )

        ImportMenu ->
            ( { model | menu = Import, state = View }, Cmd.none )

        ExportMenu ->
            ( { model | menu = Export, state = View }, Cmd.none )

        IterateMenu ->
            ( { model | menu = Iterate }, Cmd.none )


updateFileMsg : FileMsgType -> Model -> ( Model, Cmd Msg )
updateFileMsg msg model =
    case msg of
        DownloadSVG ->
            ( model, downloadSVG () )

        ExportFsk ->
            ( model, downloadFSK (Fractal.toString model.fractalModel.fractaldata False) False )

        ExportFsk2 ->
            ( model, downloadFSK (Fractal.toString model.fractalModel.fractaldata True) True )

        ImportFsk ->
            ( model
            , Select.file [] (Tuple.pair False >> FileFskToFractalData >> FileMsg)
            )

        ImportFsk2 ->
            ( model
            , Select.file [] (Tuple.pair True >> FileFskToFractalData >> FileMsg)
            )

        FileFskToFractalData ( useNew, file ) ->
            ( model
            , File.toString file
                |> Task.map
                    ((if useNew then
                        fsk2Decoder

                      else
                        fskDecoder
                     )
                        >> Result.withDefault Fractal.emptyFractalData
                    )
                |> Task.perform (SetFractalData >> FileMsg)
            )

        SetFractalData fd ->
            ( { model | fractalModel = model.fractalModel |> setFractalData fd }, Cmd.none )


downloadFSK : String -> Bool -> Cmd msg
downloadFSK data useNew =
    Download.string
        (if useNew then
            "fractal.fsk2"

         else
            "fractal.fsk"
        )
        ""
        data



-- fskDecoder


fsk2Decoder : String -> Result Error FractalData
fsk2Decoder s =
    Csv.parseWith " " s
        |> (\{ headers, records } ->
                List.head headers
                    |> Maybe.withDefault ""
                    |> Decode.decodeString bool
                    |> Result.andThen
                        (\propagateLineWeight ->
                            records
                                |> Result.Extra.combineMap parseLineDataNew
                                |> Result.andThen
                                    (\segs ->
                                        Result.Ok { seg = segs, propagateLineWeight = propagateLineWeight }
                                    )
                        )
           )


fskDecoder : String -> Result Error FractalData
fskDecoder s =
    Csv.splitWith " " s
        |> Result.Extra.combineMap parseLineDataOld
        |> Result.andThen
            (\segs ->
                Result.Ok { seg = segs, propagateLineWeight = False }
            )


type alias MousePos =
    { layerX : Float
    , layerY : Float
    , which : Int
    }


type alias KeyIn =
    { key : String }


keyboardDecoder : Decoder KeyIn
keyboardDecoder =
    Decode.map KeyIn (at [ "key" ] string)


mouseDecoder : Decoder MousePos
mouseDecoder =
    map3 MousePos
        (at [ "layerX" ] float)
        (at [ "layerY" ] float)
        (at [ "which" ] int)


type alias Touches =
    { changedTouches : List SingleTouch
    , targetTouches : List SingleTouch
    , touches : List SingleTouch
    }


type alias SingleTouch =
    { pagePosX : Float
    , pagePosY : Float
    }


touchDecoder : Decoder Touches
touchDecoder =
    let
        singleTouchDecoder =
            Decode.map2 SingleTouch
                (Decode.field "pageX" Decode.float)
                (Decode.field "pageY" Decode.float)
    in
    Decode.map3 Touches
        (Decode.field "changedTouches" <| dynamicListDecoder singleTouchDecoder)
        (Decode.field "targetTouches" <| dynamicListDecoder singleTouchDecoder)
        (Decode.field "touches" <| dynamicListDecoder singleTouchDecoder)


dynamicListDecoder : Decoder a -> Decoder (List a)
dynamicListDecoder itemDecoder =
    Decode.field "length" Decode.int
        |> Decode.andThen
            (\i ->
                List.range 0 (i - 1)
                    |> List.map (\j -> Decode.field (String.fromInt j) itemDecoder)
                    |> List.foldr (Decode.map2 (::)) (Decode.succeed [])
            )


convertDecoder : Decoder Touches -> Decoder MousePos
convertDecoder touchDec =
    touchDec
        |> Decode.map
            (\td ->
                let
                    firstTouch =
                        List.head td.targetTouches
                in
                case firstTouch of
                    Nothing ->
                        { which = 1
                        , layerX = 0
                        , layerY = 0
                        }

                    Just f ->
                        { which = 1
                        , layerX = f.pagePosX
                        , layerY = f.pagePosY
                        }
            )


segclickhandler : Int -> List (Svg.Attribute Msg)
segclickhandler i =
    [ Events.custom "contextmenu" (Decode.map (segClick i) mouseDecoder)
    , Events.custom "dblclick" (Decode.map (segDblClick i) mouseDecoder)
    , Events.custom "click" (Decode.map noMsg mouseDecoder)
    ]


segDblClick :
    Int
    -> MousePos
    ->
        { message : Msg
        , stopPropagation : Bool
        , preventDefault : Bool
        }
segDblClick i x =
    { message = EventsMsg (SegDblClick ( x, i ))
    , stopPropagation = True
    , preventDefault = True
    }


segClick :
    Int
    -> MousePos
    ->
        { message : Msg
        , stopPropagation : Bool
        , preventDefault : Bool
        }
segClick i x =
    { message = EventsMsg (SegClick ( x, i ))
    , stopPropagation = True
    , preventDefault = True
    }


stopPropagation :
    msg
    ->
        { message : msg
        , stopPropagation : Bool
        , preventDefault : Bool
        }
stopPropagation m =
    { message = m
    , stopPropagation = True
    , preventDefault = True
    }


pointmousedownhandler : Int -> List (Svg.Attribute Msg)
pointmousedownhandler i =
    [ Events.custom "mousedown" (Decode.map (pointMousedown i) mouseDecoder)
    , Events.custom "touchstart" (Decode.map (pointTouch i) touchDecoder)
    , Events.custom "mouseup" (Decode.map (stopPropagation << EventsMsg << MouseUp) (Decode.map (\x -> ( x, i )) mouseDecoder))
    , Events.custom "click" (Decode.map (stopPropagation << EventsMsg << MouseClick) mouseDecoder)
    , Events.custom "contextmenu" (Decode.map (pointRightClick i) mouseDecoder)
    ]


pointRightClick :
    Int
    -> MousePos
    ->
        { message : Msg
        , stopPropagation : Bool
        , preventDefault : Bool
        }
pointRightClick i x =
    { message = EventsMsg (RightClick ( x, i ))
    , stopPropagation = True
    , preventDefault = True
    }


pointMousedown :
    Int
    -> MousePos
    ->
        { message : Msg
        , stopPropagation : Bool
        , preventDefault : Bool
        }
pointMousedown i x =
    { message = EventsMsg (MouseDown ( x, i ))
    , stopPropagation = True
    , preventDefault = True
    }


pointTouch :
    Int
    -> Touches
    ->
        { message : Msg
        , stopPropagation : Bool
        , preventDefault : Bool
        }
pointTouch i x =
    { message = EventsMsg (TouchStart ( x, i ))
    , stopPropagation = True
    , preventDefault = True
    }


noMsg :
    MousePos
    ->
        { message : Msg
        , stopPropagation : Bool
        , preventDefault : Bool
        }
noMsg x =
    { message = None
    , stopPropagation = True
    , preventDefault = True
    }
