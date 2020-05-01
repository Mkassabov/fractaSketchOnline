module View exposing (..)

import Fractal exposing (..)
import Grid exposing (..)
import Html
import Html.Attributes
import Html.Events as Events
import Json.Decode as Decode exposing (..)
import Line exposing (..)
import Model exposing (..)
import Msg exposing (..)
import Point exposing (..)
import Svg exposing (..)
import Svg.Attributes as SvgA


view : Model -> Html.Html Msg
view model =
    Svg.svg
        [ SvgA.width (String.fromInt (model.canvasSize |> Tuple.first))
        , SvgA.height (String.fromInt (model.canvasSize |> Tuple.second))
        , Html.Attributes.id "svg"
        , SvgA.strokeLinecap "round"
        ]
        (case model.drawingModel.ghost of
            NoGhost ->
                [ generateSegments model.fractalModel.fractaldata model.drawingModel.level (lengthFilter model.drawingModel.accuracy)
                    |> List.concatMap (boundsFilter ( ( 0, 0 ), Tuple.mapBoth toFloat toFloat model.canvasSize ))
                    |> drawSegmentListNoArrow
                        [ SvgA.stroke "black" ]
                ]

            LastGhost ->
                [ generateSegments model.fractalModel.fractaldata (model.drawingModel.level - 1) (lengthFilter model.drawingModel.accuracy)
                    |> List.concatMap (boundsFilter ( ( 0, 0 ), Tuple.mapBoth toFloat toFloat model.canvasSize ))
                    |> drawSegmentListNoArrow
                        [ SvgA.stroke "black", SvgA.opacity "0.5" ]
                , generateSegments model.fractalModel.fractaldata model.drawingModel.level (lengthFilter model.drawingModel.accuracy)
                    |> List.concatMap (boundsFilter ( ( 0, 0 ), Tuple.mapBoth toFloat toFloat model.canvasSize ))
                    |> drawSegmentListNoArrow
                        [ SvgA.stroke "black" ]
                ]

            AllGhost ->
                List.range 0 model.drawingModel.level
                    |> List.map (\i -> generateSegments model.fractalModel.fractaldata i (lengthFilter model.drawingModel.accuracy))
                    |> truncateList (List.all (\s -> s.segType == Static))
                    |> (::) []
                    |> mapIndex
                        (\totalLevels level segmentList ->
                            segmentList
                                |> List.concatMap (boundsFilter ( ( 0, 0 ), Tuple.mapBoth toFloat toFloat model.canvasSize ))
                                |> drawSegmentListNoArrow
                                    [ SvgA.stroke "black", SvgA.opacity (String.fromFloat (toFloat level / toFloat (totalLevels - 1))) ]
                        )
                    |> List.reverse
        )


liveView : Model -> Html.Html Msg
liveView model =
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
        [ generateSegments (model.fractalModel.fractaldata |> addMaybePioint model.drawingModel.extraPoint |> clearDecorations ) 3 (lengthFilter model.drawingModel.accuracy)
            |> List.concatMap (boundsFilter ( ( 0, 0 ), Tuple.mapBoth toFloat toFloat model.canvasSize ))
            |> drawSegmentListNoArrow
                [ SvgA.opacity "1" ]
            
            
        , gridSVG model.drawingModel.grid model.canvasSize 25
        , drawFractalData segclickhandler model.fractalModel.fractaldata model.drawingModel.extraPoint
        , drawPointList (pointmousedownhandler -1) pointmousedownhandler (List.map (\( p, _ ) -> p) model.fractalModel.fractaldata.seg)
         
        ]


generateSegments : FractalData -> Int -> (Segment -> List Segment) -> List Segment
generateSegments fractal level filter =
    if level < 0 then
        []

    else if level == 0 then
        applyToInterval fractal (ends fractal) identity identity identity |> List.concatMap hiddenFilter

    else
        generateSegments fractal (level - 1) filter |> applyToSegmentList fractal |> List.concatMap filter


lengthFilter : Float -> Segment -> List Segment
lengthFilter len s =
    if lenSegment s > len then
        [ s ]

    else
        [ { s | segType = Static } ]


hiddenFilter : Segment -> List Segment
hiddenFilter s =
    case s.segType of
        Hidden ->
            []

        _ ->
            [ s ]


boundsFilter : ( ( Float, Float ), ( Float, Float ) ) -> Segment -> List Segment
boundsFilter bounds s =
    if s |> Line.inBounds bounds then
        [ s ]

    else
        []


truncateList : (a -> Bool) -> List a -> List a
truncateList filter list =
    case list of
        [] ->
            []

        h :: li ->
            if filter h then
                [ h ]

            else
                h :: truncateList filter li


mapIndex : (Int -> Int -> a -> b) -> List a -> List b
mapIndex func list =
    List.indexedMap (func (List.length list)) list
