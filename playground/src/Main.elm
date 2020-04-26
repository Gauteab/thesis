module Main exposing (..)

import Browser exposing (element)
import Element exposing (Element, alignBottom, alignTop, column, el, rgb, rgb255, row, spacing, text)
import Element.Background as Background
import Tree as Tree exposing (Tree, restructure)
import Tree.Zipper as Zipper exposing (Zipper)


node_ v c =
    Tree.tree (Label v True) c


node v c =
    Tree.tree (Label v False) c


testTree =
    node 1
        [ node 2
            [ node 7 []
            , node 6 []
            ]
        , node 3 []
        , node 4 []
        ]


z =
    Zipper.fromTree testTree
        |> Zipper.forward
        |> Maybe.map (Zipper.mapLabel (\l -> { l | selected = True }))
        |> Maybe.andThen Zipper.nextSibling
        |> (Maybe.map << Zipper.mapLabel) (\l -> { l | selected = True })
        |> Maybe.withDefault (Zipper.fromTree testTree)


type Msg
    = None


type alias Model =
    { zipper : Zipper Label }


view model =
    Element.layout [] <|
        render (Zipper.root z |> Zipper.tree)


update msg model =
    model


init =
    Model z


main : Program () Model Msg
main =
    element
        { view = view
        , init = \_ -> init
        , update = update
        }


type alias Label =
    { value : Int, selected : Bool }


elementFromLabel : Label -> Element.Element msg
elementFromLabel label =
    if label.selected then
        el [ Background.color (rgb255 55 210 185), alignTop ] <| text (String.fromInt label.value)

    else
        el [ alignTop ] <| text (String.fromInt label.value)


render_ : Element msg -> List (Element msg) -> Element msg
render_ label children =
    case children of
        [] ->
            label

        _ ->
            row []
                [ row [ alignTop, Element.paddingXY 5 0 ] <| [ text "(", label ]
                , column [] <| mapLast (\e -> row [] [ e, text ")" ]) children
                ]


render tree_ =
    restructure elementFromLabel render_ tree_



--main_ =
--    z
--        |> Zipper.findNext ((==) 0 << modBy 2)
--        |> Maybe.andThen (Zipper.findNext ((==) 0 << modBy 2))
--        |> Maybe.andThen (Zipper.findNext ((==) 0 << modBy 2))
-- Utility


mapLast : (a -> a) -> List a -> List a
mapLast f list =
    case list of
        [] ->
            []

        [ x ] ->
            [ f x ]

        x :: xs ->
            x :: mapLast f xs
