module Grid exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html.Attributes exposing (..)

type GridType
    = None
    | Square
    | Hex



dotSize : Float
dotSize =
    2


handleOverflow : Float -> Float -> Float -> Float
handleOverflow sc vp el =
    if sc > vp then
        el - (sc - vp)

    else
        el


cartesianProduct : List a -> List b -> List ( a, b )
cartesianProduct xList yList =
    List.concatMap
        (\x -> List.map (\y -> ( x, y )) yList)
        xList


multN : Float -> Float -> List Float
multN x n =
    List.map (\i -> toFloat i * n) (List.range 0 (1 + round (x / n)))


shiftList : List ( Float, Float ) -> ( Float, Float ) -> List ( Float, Float )
shiftList list ( x, y ) =
    List.concatMap (\( i, j ) -> [ ( i, j ), ( i + x, j + y ) ]) list


filterList : List ( Float, Float ) -> ( Int, Int ) -> List ( Float, Float )
filterList list ( x, y ) =
    List.filter (\( i, j ) -> (i > dotSize) && (i < toFloat x - dotSize) && (j > dotSize) && (j < toFloat y - dotSize)) list


squareGridPoints : Int -> Int -> Float -> List ( Float, Float )
squareGridPoints x y step =
    cartesianProduct (multN (toFloat x) step) (multN (toFloat y) step)


hexGridPoints : Int -> Int -> Float -> List ( Float, Float )
hexGridPoints x y step =
    shiftList
        (cartesianProduct (multN (toFloat x) step) (multN (toFloat y) (step * sqrt 3)))
        ( step / 2,  step * sqrt 3 / 2 )


gridSVG : GridType -> ( Int, Int ) -> Float -> Svg msg
gridSVG gt ( x, y ) step =
    Svg.g [Html.Attributes.id "points"]
        (List.map (\( i, j ) -> Svg.circle [ cx (String.fromFloat i), cy (String.fromFloat j), r (String.fromFloat dotSize) ] [])
            (filterList
                (case gt of
                    None ->
                        []

                    Square ->
                        squareGridPoints x y step

                    Hex ->
                        hexGridPoints x y step
                )
                ( x, y )
            )
        )


gridSnap : GridType -> ( Float, Float ) -> ( Float, Float )
gridSnap gt ( x, y ) =
    case gt of
        None ->
            ( x, y )

        Square ->
            ( toFloat (round (x / 25)) * 25, toFloat (round (y / 25)) * 25 )

        Hex ->
            (\( m, n ) -> ( 25 * m + 12.5 * n, 12.5 * sqrt 3 * n ))
                ( toFloat (round ((3 * x - sqrt 3 * y) / 75)), toFloat (round (y * sqrt 12 / 75)) )
