module Controls exposing (..)

import ColorPicker exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Element.Border as Border
import Element.Events as Events
import Fractal exposing (..)
import Grid exposing (..)
import Line exposing (..)
import Model exposing (..)
import Msg exposing (Msg)
import Styles exposing (..)


selectedColor : a -> a -> Color
selectedColor b c =
    if b == c then
        green_selected

    else
        green


generateGenericToggle : Bool -> String -> (Bool -> msg) -> Element msg
generateGenericToggle state label msg =
    Input.button
        (buttonCSS
            [ Background.color
                (if state then
                    green_selected

                 else
                    green
                )
            ]
            False
        )
        { onPress = Just (msg (not state))
        , label = text label
        }


generateGenericToggleList : List ( a, String ) -> (a -> msg) -> a -> Element msg
generateGenericToggleList enum msg item =
    Element.column (subContainerCSS [])
        (List.map
            (\( gtype, name ) ->
                Input.button (buttonCSS [ Background.color (selectedColor item gtype) ] True)
                    { onPress = Just (msg gtype)
                    , label = text name
                    }
            )
            enum
        )


generateLineControl : Model -> (LineType -> msg) -> LineType -> Element msg
generateLineControl model msg item =
    Element.column []
        [ Element.row (subContainerCSS [ padding 0 ])
            (List.map
                (\( lineType, name, shortName ) ->
                    Input.button (buttonCSS [ Background.color (selectedColor item lineType) ] True)
                        { onPress = Just (msg lineType)
                        , label =
                            text
                                (if isMobile model.windowSize then
                                    shortName

                                 else
                                    name
                                )
                        }
                )
                enumLineType
            )
        ]


generateGenericControls : Bool -> List ( String, msg ) -> Element msg
generateGenericControls row msgs =
    (msgs
        |> List.map
            (\( txt, msg ) ->
                Input.button (buttonCSS [ Background.color green ] True)
                    { onPress = Just msg
                    , label = Element.text txt
                    }
            )
    )
        |> (\m ->
                if row then
                    Element.column [ width fill ]
                        [ Element.row (subContainerCSS [ width fill, padding 0 ])
                            m
                        ]

                else
                    Element.column (subContainerCSS [])
                        m
           )


generateSlider : String -> Float -> Float -> (Float -> msg) -> Float -> Float -> Element msg
generateSlider label max min onChange step value =
    Element.column [ width fill, height fill, centerY, centerX, padding (0.5 |> rem) ]
        [ Input.slider
            [ Element.height (((2 |> rem) + 2) |> px)
            , Element.behindContent
                (Element.el
                    [ Element.width Element.fill
                    , Element.height (Element.px 2)
                    , Element.centerY
                    , Background.color green_selected
                    ]
                    Element.none
                )
            ]
            { label = Input.labelLeft [ paddingXY 0 (0.5 |> rem), Font.color black, Font.justify ] (text (label ++ ": "))
            , max = max
            , min = min
            , onChange = onChange
            , step = Just step
            , thumb = Input.thumb [ width ((0.75 |> rem) |> px), height ((0.75 |> rem) |> px), Background.color green ]
            , value = value
            }
        ]


generateColorControl : Model -> Element.Color -> Element Msg
generateColorControl model color =
    Element.el
        [ width (px (2 |> rem))
        , height (px (2 |> rem))
        , Background.color color
        , Border.color Styles.green
        , Border.solid
        , Border.width 2
        , Events.onClick (Msg.ShowColorPicker (not model.showColorPicker))
        , pointer
        ]
        Element.none


enumGhostType : List ( Ghost, String )
enumGhostType =
    [ ( NoGhost, "None" )
    , ( LastGhost, "Last" )
    , ( AllGhost, "All" )
    ]


enumGridType : List ( GridType, String )
enumGridType =
    [ ( None, "None" )
    , ( Square, "Square" )
    , ( Hex, "Hex" )
    ]


enumLineType : List ( LineType, String, String )
enumLineType =
    [ ( TopRight, "TopRight", "TR" )
    , ( TopLeft, "TopLeft", "TL" )
    , ( BottomRight, "BottomRight", "BR" )
    , ( BottomLeft, "BottomLeft", "BL" )
    , ( Static, "Static", "S" )
    , ( Hidden, "Hidden", "H" )
    ]


countStaticSegments : FractalData -> Int
countStaticSegments fractal =
    segments fractal |> List.filter (\s -> s.segType == Static) |> List.length


countActiveSegments : FractalData -> Int
countActiveSegments fractal =
    segments fractal |> List.filter (\s -> (s.segType /= Hidden) && (s.segType /= Static)) |> List.length


countSegments : FractalData -> Int -> ( Int, Int )
countSegments fractal level =
    if level < 0 then
        ( 0, 0 )

    else if level == 0 then
        ( countStaticSegments fractal, countActiveSegments fractal )

    else
        countSegments fractal (level - 1) |> (\( static, active ) -> ( static + countStaticSegments fractal * active, countActiveSegments fractal * active ))
