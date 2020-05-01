module Fractal exposing (..)

import Element exposing (Color, rgb255)
import Html.Attributes exposing (..)
import Line exposing (..)
import Point exposing (..)
import Styles exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias FractalData =
    { seg : List ( Point, Maybe LineData )
    , propagateLineWeight : Bool
    }


clearDecorations : FractalData -> FractalData
clearDecorations f =
    { f | propagateLineWeight = False, seg = f.seg |> List.map (\( p, mld ) -> ( p, Maybe.map (\ld -> { ld | color = Styles.pink, weight = 0.1 }) mld )) }


addMaybePioint : Maybe ( Point, LineData ) -> FractalData -> FractalData
addMaybePioint xp f =
    case xp of
        Nothing ->
            f

        Just ( p, lineData ) ->
            { f | seg = f.seg ++ [ ( p, Just lineData ) ] }


emptyFractalData : FractalData
emptyFractalData =
    { seg = [], propagateLineWeight = False }


setPropagateLineWeight : Bool -> FractalData -> FractalData
setPropagateLineWeight value f =
    { f | propagateLineWeight = value }


addPoint : FractalData -> LineData -> Point -> FractalData
addPoint f lineData p =
    case f.seg of
        [] ->
            { f | seg = [ ( p, Nothing ) ] }

        _ ->
            { f | seg = f.seg ++ [ ( p, Just lineData ) ] }


splitLine : FractalData -> Int -> FractalData
splitLine f i =
    if i <= 0 then
        f

    else if i >= List.length f.seg then
        f

    else
        { f
            | seg =
                List.take i f.seg
                    ++ (List.drop (i - 1) (segments f) |> List.head |> midPointList)
                    ++ List.drop i f.seg
        }


fracRotateSeg : Int -> FractalData -> FractalData
fracRotateSeg i f =
    if i < 0 then
        f

    else if i >= List.length f.seg then
        f

    else
        { f
            | seg =
                List.take i f.seg
                    ++ (List.drop i f.seg
                            |> (\l ->
                                    case l of
                                        [] ->
                                            []

                                        ( q, t ) :: h ->
                                            ( q, t |> Maybe.map rotateLineData ) :: h
                               )
                       )
        }


movePoint : FractalData -> Int -> Point -> FractalData
movePoint f i p =
    if i < 0 then
        f

    else if i >= List.length f.seg then
        f

    else
        { f
            | seg =
                List.take i f.seg
                    ++ (List.drop i f.seg
                            |> (\l ->
                                    case l of
                                        [] ->
                                            []

                                        ( q, t ) :: h ->
                                            ( p, t ) :: h
                               )
                       )
        }


getPoint : FractalData -> Int -> Point
getPoint f i =
    if i < 0 then
        ( 0, 0 )

    else if i >= List.length f.seg then
        ( 0, 0 )

    else
        List.drop i f.seg |> List.head |> Maybe.map Tuple.first |> Maybe.withDefault ( 0, 0 )


removePoint : FractalData -> Int -> FractalData
removePoint f i =
    if i < 0 then
        f

    else if i >= List.length f.seg then
        f

    else if i == 0 then
        { f
            | seg =
                case f.seg of
                    _ :: ( p, _ ) :: l ->
                        ( p, Nothing ) :: l

                    _ ->
                        []
        }

    else
        { f | seg = List.take i f.seg ++ List.drop (i + 1) f.seg }


ends : FractalData -> Maybe ( Point, Point )
ends f =
    case f.seg of
        [] ->
            Nothing

        ( pf, _ ) :: ss ->
            List.foldl (Just >> always) Nothing f.seg |> Maybe.map (\( pl, _ ) -> ( pf, pl ))


span : FractalData -> Maybe Point
span f =
    ends f |> Maybe.map (\( a, b ) -> diffPoint a b)


addtoSegments : ( Point, Maybe LineData ) -> ( List Segment, Point ) -> ( List Segment, Point )
addtoSegments ( np, ld ) ( sl, lp ) =
    case ld of
        Nothing ->
            ( sl, np )

        Just lineData ->
            ( sl
                ++ [ { startPoint = lp
                     , endPoint = np
                     , segType = lineData.lineType
                     , weight = lineData.weight
                     , color = lineData.color
                     }
                   ]
            , np
            )


segments : FractalData -> List Segment
segments f =
    case f.seg of
        [] ->
            []

        ( p, _ ) :: listEnd ->
            List.foldl addtoSegments ( [], p ) listEnd |> (\( a, _ ) -> a)


drawFractalData : (Int -> List (Attribute msg)) -> FractalData -> Maybe ( Point, LineData ) -> Svg.Svg msg
drawFractalData aFunc f xp =
    Svg.g []
        (segments
            (addMaybePioint xp f)
            |> (\list ->
                    List.map2
                        (\s n ->
                            drawSegment
                                (if n < List.length f.seg then
                                    aFunc n

                                 else
                                    [ Html.Attributes.id "extraline" ]
                                )
                                s
                        )
                        list
                        (List.range 1 (List.length list))
               )
        )


transform : Maybe ( Point, Point ) -> Maybe ( Point, Point ) -> (Point -> Point) -> Point -> Point
transform u v =
    Maybe.map2
        (\( a, b ) ( p, q ) ->
            \flip x ->
                diffPoint a x
                    |> prodPoint (invertPoint (diffPoint a b))
                    |> flip
                    |> prodPoint (diffPoint p q)
                    |> sumPoint p
        )
        u
        v
        |> Maybe.withDefault (\_ -> identity)


applyToInterval : FractalData -> Maybe ( Point, Point ) -> (Point -> Point) -> (LineType -> LineType) -> (Float -> Float) -> List Segment
applyToInterval f i flip changeType changeWeight =
    List.map
        (\s ->
            { s
                | startPoint = transform (ends f) i flip s.startPoint
                , endPoint = transform (ends f) i flip s.endPoint
                , segType = changeType s.segType
                , weight = changeWeight s.weight
            }
        )
        (segments f)


applyToSegment : FractalData -> Segment -> List Segment
applyToSegment f s =
    let
        changeWeight =
            if f.propagateLineWeight then
                \w -> Basics.max 0.025 (1.25 * s.weight * w)

            else
                identity
    in
    case s.segType of
        Hidden ->
            []

        Static ->
            [ s ]

        TopRight ->
            applyToInterval f (Just ( s.startPoint, s.endPoint )) identity identity changeWeight

        TopLeft ->
            applyToInterval f (Just ( s.endPoint, s.startPoint )) flipPoint flipSeg changeWeight

        BottomLeft ->
            applyToInterval f (Just ( s.endPoint, s.startPoint )) identity identity changeWeight

        BottomRight ->
            applyToInterval f (Just ( s.startPoint, s.endPoint )) flipPoint flipSeg changeWeight


applyToSegmentList : FractalData -> List Segment -> List Segment
applyToSegmentList f list =
    List.filter (\s -> s.segType /= Hidden) (List.concatMap (applyToSegment f) list)


removeShortSegments : List Segment -> List Segment
removeShortSegments list =
    List.map
        (\s ->
            if lenSegment s > 2 then
                s

            else
                { s | segType = Static }
        )
        list


scalePoint : FractalData -> Float -> Point -> Point
scalePoint fractal scaleFactor =
    case ends fractal of
        Nothing ->
            identity

        Just ( a, b ) ->
            let 
                mid = midPoint a b
                invmid =
                    multPoint -1 mid
            in
                translatePoint invmid >> multPoint scaleFactor >> translatePoint mid

scaleTemplate : FractalData -> Float -> FractalData
scaleTemplate fractal scaleFactor =
    { fractal
        | seg =
            fractal.seg
                |> List.map
                    (\( point, lineType ) ->
                        ( scalePoint fractal scaleFactor point, lineType )
                    )
    }


rotatePoint : FractalData -> Float -> Point -> Point
rotatePoint fractal rotateFactor =
    let
        rot =
            (\p -> toPolar p |> (\( r, theta ) -> fromPolar ( r, theta + degrees rotateFactor )))
    in
    case ends fractal of
        Nothing ->
            identity

        Just ( a, b ) ->
            let
                mid =
                    midPoint a b

                invmid =
                    multPoint -1 mid
            in
                translatePoint invmid >> rot >> translatePoint mid


rotateTemplate : FractalData -> Float -> FractalData
rotateTemplate fractal rotateFactor =
    { fractal
        | seg =
            fractal.seg
                |> List.map
                    (\( point, lineType ) ->
                        ( rotatePoint fractal rotateFactor point, lineType )
                    )
    }


moveTemplate : FractalData -> ( Float, Float ) -> FractalData
moveTemplate fractal translate =
    { fractal
        | seg =
            fractal.seg
                |> List.map
                    (\( point, lineType ) ->
                        ( translatePoint translate point, lineType )
                    )
    }


allowInfiniteIteration : FractalData -> Bool
allowInfiniteIteration f =
    let
        max =
            span f |> Maybe.map lenPoint |> Maybe.withDefault 0
    in
    segments f |> List.all (\s -> lenSegment s < max)


toString : FractalData -> Bool -> String
toString f useNew =
    (if useNew then
        boolToString f.propagateLineWeight ++ "\n"

     else
        ""
    )
        ++ String.concat
            (List.map (Line.toString useNew) f.seg)


boolToString : Bool -> String
boolToString bool =
    if bool then
        "true"

    else
        "false"
