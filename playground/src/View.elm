module View exposing (..)

import Element exposing (Element, alignTop, column, el, px, rgb255, row, spacing, text)
import Element.Background as Background
import Element.Font as Font exposing (Font)
import Html exposing (Html)



-- VIEW


tab =
    --text "  "
    el [ Element.width (px 15) ] Element.none


keyword string =
    el [ Font.color (rgb255 137 89 168) ] (text string)


grid columnStyle rowStyle elements =
    column columnStyle <| List.map (row rowStyle) elements


editorLayout : Element msg -> Html msg
editorLayout =
    Element.layout
        [ Font.family [ Font.monospace ]
        , Font.color (rgb255 0 0 0)
        , Element.paddingXY 5 5
        ]


highlighted : Bool -> Element msg -> Element msg
highlighted selected =
    el (style selected)


style selected =
    if selected then
        [ Background.color (rgb255 55 210 185), alignTop ]

    else
        [ alignTop ]


iff render p x y =
    grid [] [ spacing 4 ] <|
        [ [ keyword "if", render p, keyword "then" ]
        , [ tab, render x ]
        , [ keyword "else" ]
        , [ tab, render y ]
        ]


assignment render name arguments expression =
    grid [] [ spacing 4 ] <|
        [ [ text name, row [ spacing 4 ] (List.map text arguments), text "=" ]
        , [ tab, render expression ]
        ]


assignment_ render name expression =
    grid [] [ spacing 4 ] <|
        [ [ text name, text "=" ]
        , [ tab, render expression ]
        ]


identifier name =
    text name


list render children =
    case children of
        [] ->
            text "[]"

        e :: es ->
            row [] <|
                [ row [] [ text "[", render e ]
                , row [] <| List.map (\x -> row [] [ text ",", render x ]) es
                , text "]"
                ]


hole =
    el [ Background.color (rgb255 255 110 110) ] (text "_")


module_ render declarations =
    column [] (List.map render declarations)
