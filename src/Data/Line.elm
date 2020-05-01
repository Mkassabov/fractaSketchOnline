module Line exposing (..)

import Element exposing (Color, toRgb)
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode
import Point exposing (..)
import Result.Extra
import Styles exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)



-- Lines


type alias LineData =
    { lineType : LineType
    , weight : Float
    , color : Element.Color
    }


type LineType
    = TopRight
    | TopLeft
    | BottomRight
    | BottomLeft
    | Static
    | Hidden


lookup : List ( a, b ) -> a -> Result String b
lookup l a =
    case l of
        [] ->
            Result.Err "Not Found"

        ( xa, xb ) :: xs ->
            if xa == a then
                Result.Ok xb

            else
                lookup xs a


revLookup : List ( a, b ) -> b -> Result String a
revLookup l b =
    case l of
        [] ->
            Result.Err "Not Found"

        ( xa, xb ) :: xs ->
            if xb == b then
                Result.Ok xa

            else
                revLookup xs b


flipSeg : LineType -> LineType


flipSeg t =
    case t of
        TopRight ->
            BottomRight

        TopLeft ->
            BottomLeft

        BottomRight ->
            TopRight

        BottomLeft ->
            TopLeft

        Static ->
            Static

        Hidden ->
            Hidden


rotateLineType : LineType -> LineType
rotateLineType t =
    case t of
        TopRight ->
            TopLeft

        TopLeft ->
            BottomRight

        BottomRight ->
            BottomLeft

        BottomLeft ->
            Static

        Static ->
            Hidden

        Hidden ->
            TopRight



-- Segments


type DrawType
    = Seg
    | HiddenSeg
    | Arrow


type alias Segment =
    { segType : LineType
    , color : Element.Color
    , weight : Float
    , startPoint : Point
    , endPoint : Point
    }


inBounds : ( ( Float, Float ), ( Float, Float ) ) -> Segment -> Bool
inBounds bounds s =
    Point.inBounds bounds s.startPoint
        || Point.inBounds bounds s.endPoint


rotateLineData : LineData -> LineData
rotateLineData ld =
    { ld | lineType = rotateLineType ld.lineType }


getSegment : Segment -> ( Point, Point )
getSegment s =
    ( s.startPoint, s.endPoint )


midPointList : Maybe Segment -> List ( Point, Maybe LineData )
midPointList ms =
    -- ms |> Maybe.map (\s -> [ ( midPoint s.startPoint s.endPoint, Just {lineType = s.segType, weight = s.weight, color = s.color} ) ]) |> Maybe.withDefault []
    case ms of
        Nothing ->
            []

        Just s ->
            [ ( midPoint s.startPoint s.endPoint, Just { lineType = s.segType, weight = s.weight, color = s.color } ) ]


lenSegment : Segment -> Float
lenSegment s =
    diffPoint s.startPoint s.endPoint |> lenPoint


getSegementList : Segment -> List ( ( Point, Point ), DrawType )
getSegementList s =
    case s.segType of
        TopRight ->
            [ ( getSegment s, Seg )
            , ( ( s.endPoint, sumPoint s.endPoint (rotatePoint (diffPoint s.startPoint s.endPoint) (degrees -135)) ), Arrow )
            ]

        TopLeft ->
            [ ( getSegment s, Seg )
            , ( ( s.startPoint, sumPoint s.startPoint (rotatePoint (diffPoint s.startPoint s.endPoint) (degrees -45)) ), Arrow )
            ]

        BottomRight ->
            [ ( getSegment s, Seg )
            , ( ( s.endPoint, sumPoint s.endPoint (rotatePoint (diffPoint s.startPoint s.endPoint) (degrees 135)) ), Arrow )
            ]

        BottomLeft ->
            [ ( getSegment s, Seg )
            , ( ( s.startPoint, sumPoint s.startPoint (rotatePoint (diffPoint s.startPoint s.endPoint) (degrees 45)) ), Arrow )
            ]

        Static ->
            [ ( getSegment s, Seg ) ]

        Hidden ->
            [ ( getSegment s, HiddenSeg ) ]



-- Draw Segments


drawSegment : List (Attribute msg) -> Segment -> Svg.Svg msg
drawSegment a s =
    Svg.g []
        (List.map
            (\( ( ( x, y ), ( p, q ) ), t ) ->
                Svg.g []
                    [ if t == Seg || t == HiddenSeg then
                        Svg.line
                            ([ x1 (String.fromFloat x)
                             , y1 (String.fromFloat y)
                             , x2 (String.fromFloat p)
                             , y2 (String.fromFloat q)
                             , stroke "pink"
                             , opacity "0"
                             , strokeWidth (String.fromFloat (s.weight + 0.75) ++ "rem")
                             ]
                                ++ a
                            )
                            []

                      else
                        Svg.text ""
                    , Svg.line
                        ([ x1 (String.fromFloat x)
                         , y1 (String.fromFloat y)
                         , x2 (String.fromFloat p)
                         , y2 (String.fromFloat q)
                         , stroke
                            (if t == Arrow then
                                "red"

                             else
                                colorString s.color
                            )
                         , strokeDasharray
                            (if t == HiddenSeg then
                                "5,20,10,20"

                             else
                                ""
                            )
                         , strokeWidth (String.fromFloat s.weight ++ "rem")
                         , strokeOpacity
                            (if t == Arrow then
                                "1"

                             else
                                String.fromFloat (s.color |> toRgb).alpha
                            )
                         ]
                            ++ (if t == Arrow then
                                    []

                                else
                                    a
                               )
                        )
                        []
                    ]
            )
            (getSegementList s)
        )


drawSegmentNoArrow : Segment -> Svg.Svg msg
drawSegmentNoArrow s =
    (\( ( x, y ), ( p, q ) ) ->
        Svg.line
            [ x1 (String.fromFloat x)
            , y1 (String.fromFloat y)
            , x2 (String.fromFloat p)
            , y2 (String.fromFloat q)
            , strokeWidth (String.fromFloat s.weight ++ "rem")
            , stroke (colorString s.color)
            , strokeOpacity (String.fromFloat (s.color |> toRgb).alpha)
            ]
            []
    )
        ( s.startPoint, s.endPoint )


drawSegmentList : (Int -> List (Attribute msg)) -> List Segment -> Svg.Svg msg
drawSegmentList attrs l =
    Svg.g []
        (List.map2 (\s n -> drawSegment (attrs n) s) l (List.range 1 (List.length l)))


drawSegmentListNoArrow : List (Svg.Attribute msg) -> List Segment -> Svg.Svg msg
drawSegmentListNoArrow attrs segs =
    Svg.g attrs
        (List.map drawSegmentNoArrow segs)


lineTypeToIntLookUp : List ( LineType, Int )
lineTypeToIntLookUp =
    [ ( TopRight, 1 ), ( TopLeft, 3 ), ( BottomRight, 2 ), ( BottomLeft, 4 ), ( Static, 5 ), ( Hidden, 6 ) ]


toStringLineData : LineData -> Bool -> String
toStringLineData data useNew = 
    let
        color =
            toRgb data.color
    in
    (data.lineType |> lookup lineTypeToIntLookUp |> Result.withDefault 0 |> String.fromInt)
        ++ (if useNew then
                " " ++ 
                    String.fromFloat data.weight
                    ++ " "
                    ++ String.fromFloat color.red
                    ++ " "
                    ++ String.fromFloat color.green
                    ++ " "
                    ++ String.fromFloat color.blue
                    ++ " "
                    ++ String.fromFloat color.alpha

            else
                ""
           )


toString : Bool-> ( Point, Maybe LineData ) -> String
toString useNew (point , mdata)  =
    (case mdata of
        Just data -> (toStringLineData data useNew ++ " ")
        Nothing -> ""
    ) 
    ++ Point.toString point
    ++ "\n"


getLineType : Float -> Result String LineType
getLineType t =
    Basics.round t |> revLookup lineTypeToIntLookUp |> Result.mapError (\_ -> "Unsupported Line Type")


parseLineDataNew : List String -> Result Error ( Point, Maybe LineData )
parseLineDataNew line =
    case line |> List.map (Decode.decodeString Decode.float) of
        rt :: rw :: rr :: rg :: rb :: ra :: rx :: ry :: rjunk ->
            Result.Extra.combineBoth ( rx, ry )
                |> Result.andThen
                    (\p ->
                        Result.Extra.combine [ rt, rw, rr, rg, rb, ra ]
                            |> Result.andThen
                                (\data ->
                                    case data of
                                        t :: w :: r :: g :: b :: a :: junk ->
                                            t
                                                |> getLineType
                                                |> Result.mapError (\er -> Decode.Failure er Encode.null)
                                                |> Result.andThen
                                                    (\lt ->
                                                        Result.Ok
                                                            ( p
                                                            , Just
                                                                { lineType = lt
                                                                , weight = w
                                                                , color = Element.rgba r g b a
                                                                }
                                                            )
                                                    )

                                        _ ->
                                            Result.Err (Decode.Failure "WTF????" Encode.null)
                                )
                    )

        rx :: ry :: rjunk ->
            Result.Extra.combineBoth ( rx, ry ) |> Result.andThen (\p -> Result.Ok ( p, Nothing ))

        _ ->
            Result.Err (Decode.Failure "too few fields" Encode.null)


parseLineDataOld : List String -> Result Error ( Point, Maybe LineData )
parseLineDataOld line =
    case line |> List.map (Decode.decodeString Decode.float) of
        rt :: rx :: ry :: rjunk ->
            Result.Extra.combineBoth ( rx, ry )
                |> Result.andThen
                    (\p ->
                        rt |> Result.andThen
                                (\t ->t
                                        |> getLineType
                                        |> Result.mapError (\er -> Decode.Failure er Encode.null)
                                        |> Result.andThen
                                            (\lt ->
                                                Result.Ok
                                                    ( p
                                                    , Just
                                                        { lineType = lt
                                                        , weight = 0.25
                                                        , color = Styles.black
                                                        }
                                                    )
                                            )
                                )
                    )

        rx :: ry :: rjunk ->
            Result.Extra.combineBoth ( rx, ry ) |> Result.andThen (\p -> Result.Ok ( p, Nothing ))

        _ ->
            Result.Err (Decode.Failure "too few fields" Encode.null)
