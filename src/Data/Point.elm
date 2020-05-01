module Point exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Point =
    ( Float, Float )


diffPointM : Maybe Point -> Maybe Point -> Maybe Point
diffPointM = Maybe.map2 diffPoint


diffPoint : Point -> Point -> Point
diffPoint ( x0, y0 ) ( x1, y1 ) =
    ( x1 - x0, y1 - y0 )


sumPoint : Point -> Point -> Point
sumPoint ( x0, y0 ) ( x1, y1 ) =
    ( x1 + x0, y1 + y0 )


midPoint : Point -> Point -> Point
midPoint ( x0, y0 ) ( x1, y1 ) =
    ( (x1 + x0) / 2, (y1 + y0) / 2 )


prodPoint : Point -> Point -> Point
prodPoint ( x0, y0 ) ( x1, y1 ) =
    ( x1 * x0 - y0 * y1, x0 * y1 + x1 * y0 )


invertPoint : Point -> Point
invertPoint ( x0, y0 ) =
    ( x0 / (x0 * x0 + y0 * y0), -y0 / (x0 * x0 + y0 * y0) )


flipPoint : Point -> Point
flipPoint ( x0, y0 ) =
    ( x0, 0 - y0 )


rotatePoint : Point -> Float -> Point
rotatePoint point angle =
    (\( r, theta ) -> fromPolar ( 30, theta + angle )) (toPolar point)


lenPoint : Point -> Float
lenPoint ( x, y ) =
    sqrt (x * x + y * y + 0.0001)


multPoint : Float -> Point -> Point
multPoint n ( x, y ) =
    ( n * x, n * y )

translatePoint: (Float, Float) -> Point -> Point
translatePoint (offsetX, offsetY) (x, y) =
    (x + offsetX, y + offsetY)

inBounds: ((Float, Float), (Float, Float)) -> Point -> Bool
inBounds ((minX, minY), (maxX, maxY)) ( x, y ) =
    minX <= x && x <= maxX && minY <= y && y <= maxY 

drawPoint : List (Attribute msg) -> Point -> Svg.Svg msg
drawPoint a ( x, y ) =
    Svg.g []
        [ Svg.circle
            ([ cx (String.fromFloat x)
             , cy (String.fromFloat y)
             , r (String.fromFloat 12)
             , fill "pink"
             , opacity "0"
            --  , opacity "0.5"
             ]
                ++ a
            )
            []
        , Svg.circle
            ([ cx (String.fromFloat x)
             , cy (String.fromFloat y)
             , r (String.fromFloat 5)
             , fill "blue"
             ]
                ++ a
            )
            []
        ]


drawPointList : List (Attribute msg) -> (Int -> List (Attribute msg)) -> List Point -> Svg.Svg msg
drawPointList a aa l =
    Svg.g a
        (List.map2 (\s n -> drawPoint (aa n) s) l (List.range 0 (List.length l)))

toString : Point -> String
toString (x, y) =
    String.fromFloat x ++ " " ++ String.fromFloat y
    