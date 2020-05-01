module Draw exposing (..)

import Fractal exposing (..)
import Grid exposing (..)
import Html
import Html.Attributes
import Html.Events as Events
import Json.Decode as Decode exposing (..)
import Model exposing (..)
import Msg exposing (..)
import Point exposing (..)
import Svg exposing (..)
import Svg.Attributes as SvgA


view : Model -> Html.Html Msg
view model =
    Svg.svg
        ([ SvgA.width (String.fromInt (model.canvasSize |> Tuple.first))
         , SvgA.height (String.fromInt (model.canvasSize |> Tuple.second))
         , SvgA.strokeLinecap "round"
         , Html.Attributes.id "svg"
         , Events.on "mousemove" (Decode.map (EventsMsg << MouseMove) mouseDecoder)
         , Events.on "touchmove" (Decode.map (EventsMsg << TouchMove) touchDecoder)
         , Events.custom "contextmenu" (Decode.map noMsg mouseDecoder)
         , Events.on "mouseleave" (Decode.map (EventsMsg << MouseLeave) mouseDecoder)
         ]
            ++ (if model.drawingModel.dragged == Nothing then
                    [ Events.on "click" (Decode.map (EventsMsg << MouseClick) mouseDecoder) ]

                else
                    [ Html.Attributes.style "" "" ]
               )
        )
        ((if model.state == Drawing then
            [ gridSVG model.drawingModel.grid ( model.canvasSize |> Tuple.first, model.canvasSize |> Tuple.second ) 25 ]

          else
            []
         )
            ++ [ drawFractalData segclickhandler model.fractalModel.fractaldata model.drawingModel.extraPoint
               , drawPointList (pointmousedownhandler -1) pointmousedownhandler (List.map (\( p, _ ) -> p) model.fractalModel.fractaldata.seg)
               ]
        )
