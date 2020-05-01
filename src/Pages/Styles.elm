module Styles exposing (..)

import Color exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Html.Attributes


green : Element.Color
green =
    Element.rgb255 40 167 69


green_selected : Element.Color
green_selected =
    Element.rgb255 30 126 52


white : Element.Color
white =
    Element.rgb 1 1 1


black : Element.Color
black =
    Element.rgb 0 0 0

pink : Element.Color
pink =
    Element.rgb255 255 192 203


background : Element.Color
background =
    Element.rgb255 48 68 87

convertColorType : Color.Color -> Element.Color
convertColorType color =
    fromRgb <| (color |> toRgba) 

convertColorTypeBack : Element.Color -> Color.Color 
convertColorTypeBack color =
    fromRgba <| (color |> toRgb) 

containerCSS : List (Attribute msg) -> List (Attribute msg)
containerCSS a =
    [ Background.color white, Border.color green, Border.width 3 ] ++ a


subContainerCSS : List (Attribute msg) -> List (Attribute msg)
subContainerCSS a =
    [ centerY, centerX, paddingEach edges ] ++ a


buttonCSS : List (Attribute msg) -> Bool -> List (Attribute msg)
buttonCSS a hover =
    [ width fill, paddingXY (0.6 |> rem) (0.5 |> rem), htmlAttribute (Html.Attributes.style "box-shadow" "none") ]
        ++ a
        ++ (if hover then
                [ mouseOver [ Background.color green_selected ] ]

            else
                []
           )


remPadding : String -> Attribute msg
remPadding val =
    htmlAttribute (Html.Attributes.style "padding" val)

rootSize: Int
rootSize = 16

rem : Float -> Int
rem r =
    round (r * toFloat rootSize)

colorString : Element.Color -> String
colorString color =
    let
        formatFloat =
            \n -> String.fromInt (round (n * 255))
    in
    Element.toRgb color |> (\c -> "rgba(" ++ formatFloat c.red ++ ", " ++ formatFloat c.green ++ ", " ++ formatFloat c.blue ++ ", " ++ formatFloat c.alpha ++ ")")

isMobile : (Int, Int) -> Bool
isMobile (x,y) =
    (classifyDevice { height = y, width = x }).class == Phone


isTablet : (Int, Int) -> Bool
isTablet (x,y) =
    (classifyDevice { height = y, width = x }).class == Tablet

edges: { top : Int, right : Int, bottom : Int, left : Int}
edges =
    { top = 0.5 |> rem
    , right = 0.5 |> rem
    , bottom = 0.5 |> rem
    , left = 0.5 |> rem
    }

noEdges: { top : Int, right : Int, bottom : Int, left : Int}
noEdges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


htmlEntity : List Int -> String
htmlEntity codes =
    codes
        |> List.map (Char.fromCode >> String.fromChar)
        |> String.join ""