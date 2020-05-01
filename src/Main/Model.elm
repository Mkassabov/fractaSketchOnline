module Model exposing (..)

import ColorPicker exposing (..)
import Element exposing (..)
import Fractal exposing (..)
import Grid exposing (..)
import Line exposing (..)
import Point exposing (..)
import Styles exposing (..)


type alias Model =
    { fractalModel : FractalModel
    , drawingModel : DrawingModel
    , colorPicker : ColorPicker.State
    , showColorPicker : Bool
    , canvasSize : ( Int, Int )
    , windowSize : ( Int, Int )
    , menu : Menu
    , state : State
    }


type alias FractalModel =
    { fractaldata : FractalData
    , lineType : LineType
    , color : Element.Color
    , lineWeight : Float
    }


getLineData : FractalModel -> LineData
getLineData m =
    { lineType = m.lineType, color = m.color, weight = m.lineWeight }


type alias DrawingModel =
    { grid : GridType
    , extraPoint : Maybe ( Point, LineData )
    , drawExtraPoint : Bool
    , dragged : Maybe Int
    , draggedCoord : Point
    , level : Int
    , ghost : Ghost
    , accuracy : Float
    , pauseIteration: Bool
    }


type State
    = Drawing
    | LiveDraw
    | Iterating
    | View


type Ghost
    = NoGhost
    | LastGhost
    | AllGhost


type Menu
    = Draw
    | Import
    | Export
    | Iterate


init : ( Int, Int ) -> Model
init window =
    { fractalModel = initFractalModel
    , drawingModel = initDrawingModel
    , colorPicker = ColorPicker.empty
    , showColorPicker = False
    , canvasSize = ( 0, 0 )
    , windowSize = window
    , state = Drawing
    , menu = Draw
    }


updateColorPicker : ColorPicker.Msg -> Model -> Model
updateColorPicker msg model =
    let
        ( state, color ) =
            ColorPicker.update msg (convertColorTypeBack model.fractalModel.color) model.colorPicker
    in
    { model
        | colorPicker = state
        , fractalModel = model.fractalModel |> setColor (Maybe.map convertColorType color |> Maybe.withDefault model.fractalModel.color)
    }


initFractalModel : FractalModel
initFractalModel =
    { fractaldata = emptyFractalData
    , lineType = TopRight
    , color = Styles.black
    , lineWeight = 0.25
    }


setFractalData : FractalData -> FractalModel -> FractalModel
setFractalData fd m =
    { m | fractaldata = fd }


setLineWeight : Float -> FractalModel -> FractalModel
setLineWeight lw m =
    { m | lineWeight = lw }


setPropagateLineWeight : Bool -> FractalModel -> FractalModel
setPropagateLineWeight value m =
    { m | fractaldata = m.fractaldata |> Fractal.setPropagateLineWeight value }


setLineType : LineType -> FractalModel -> FractalModel
setLineType ln m =
    { m | lineType = ln }


setColor : Element.Color -> FractalModel -> FractalModel
setColor color m =
    { m | color = color }


initDrawingModel : DrawingModel
initDrawingModel =
    { grid = Square
    , extraPoint = Nothing
    , drawExtraPoint = False
    , dragged = Nothing
    , draggedCoord = ( 0, 0 )
    , level = 0
    , ghost = NoGhost
    , accuracy = 2
    , pauseIteration = False
    }


setGrid : GridType -> DrawingModel -> DrawingModel
setGrid gd m =
    { m | grid = gd }


setExtraPoint : Maybe ( Point, LineData ) -> DrawingModel -> DrawingModel
setExtraPoint ep m =
    { m | extraPoint = ep }


setDrawExtraPoint : Bool -> DrawingModel -> DrawingModel
setDrawExtraPoint dep m =
    { m | drawExtraPoint = dep }


setDragged : Maybe Int -> DrawingModel -> DrawingModel
setDragged dr m =
    { m | dragged = dr }


setDraggedCoord : Point -> DrawingModel -> DrawingModel
setDraggedCoord drc m =
    { m | draggedCoord = drc }


setLevel : Int -> DrawingModel -> DrawingModel
setLevel lvl m =
    { m | level = lvl }


setGhost : Ghost -> DrawingModel -> DrawingModel
setGhost gh m =
    { m | ghost = gh }


setAccuracy : Float -> DrawingModel -> DrawingModel
setAccuracy acc m =
    { m | accuracy = acc }

setPauseIteration : Bool -> DrawingModel -> DrawingModel
setPauseIteration pi m =
    { m | pauseIteration = pi }