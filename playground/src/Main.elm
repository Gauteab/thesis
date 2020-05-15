module Main exposing (..)

import Browser
import Browser.Events
import Element exposing (Element, alignBottom, alignTop, column, el, px, rgb, rgb255, row, spacing, text)
import Element.Background as Background
import Element.Font as Font exposing (Font)
import Html exposing (Html)
import Json.Decode
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Tree as Tree exposing (Tree, restructure)
import Tree.Zipper as Zipper exposing (Zipper)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- UPDATE


type Msg
    = NoOp
    | HandleKeyboardEvent KeyboardEvent


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        HandleKeyboardEvent keyboardEvent ->
            let
                newModel : Model
                newModel =
                    keyboardEvent.key
                        --|> Debug.log "key"
                        |> Maybe.andThen keyToAction
                        --|> Debug.log "Action"
                        |> Maybe.map (doAction model)
                        --|> Debug.log "after action"
                        |> Maybe.withDefault model
            in
            ( newModel, Cmd.none )


type Direction
    = Down
    | Up
    | Right
    | Left


type Action
    = Move Direction
    | Delete


doMove : Zipper Label -> Direction -> Zipper Label
doMove zipper direction =
    let
        moveZipper : Zipper label -> Maybe (Zipper label)
        moveZipper =
            case direction of
                Down ->
                    Zipper.forward

                Up ->
                    Zipper.parent

                Right ->
                    Zipper.nextSibling

                Left ->
                    Zipper.previousSibling
    in
    zipper
        |> setSelected False
        |> moveZipper
        |> Maybe.map (setSelected True)
        |> Maybe.withDefault zipper


doAction : Model -> Action -> Model
doAction model action =
    case action of
        Move direction ->
            doMove model.zipper direction |> Model

        Delete ->
            model.zipper |> doDelete |> Model


doDelete : Zipper Label -> Zipper Label
doDelete zipper =
    let
        newZipper =
            Zipper.replaceTree (node Hole []) zipper

        parent =
            Zipper.parent newZipper |> Maybe.map Zipper.label |> Maybe.map .value

        zipperFilterChildren : (Tree a -> Bool) -> Zipper a -> Zipper a
        zipperFilterChildren =
            Zipper.mapTree << Tree.mapChildren << List.filter

        isHole : Tree Label -> Bool
        isHole =
            Tree.label >> .value >> (/=) Hole
    in
    case parent of
        Just List ->
            Zipper.parent newZipper
                |> Maybe.map (zipperFilterChildren isHole)
                |> Maybe.map (setSelected True)
                |> Maybe.withDefault newZipper

        _ ->
            newZipper


keyToAction : String -> Maybe Action
keyToAction key =
    case key of
        "l" ->
            Just <| Move Right

        "h" ->
            Just <| Move Left

        "k" ->
            Just <| Move Up

        "j" ->
            Just <| Move Down

        "d" ->
            Just <| Delete

        _ ->
            Nothing


setSelected : Bool -> Zipper Label -> Zipper Label
setSelected bool =
    Zipper.mapLabel (\label -> { label | selected = bool })



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onKeyDown <| Json.Decode.map HandleKeyboardEvent decodeKeyboardEvent



-- VIEW


render : Tree Label -> Element msg
render tree =
    let
        label =
            Tree.label tree

        children =
            Tree.children tree

        tab =
            --text "  "
            el [ Element.width (px 15) ] Element.none

        keyword string =
            el [ Font.color (rgb255 137 89 168) ] (text string)

        grid columnStyle rowStyle elements =
            column columnStyle <| List.map (row rowStyle) elements

        style =
            if label.selected then
                [ Background.color (rgb255 55 210 185), alignTop ]

            else
                [ alignTop ]
    in
    el style <|
        case ( label.value, children ) of
            ( If, [ p, x, y ] ) ->
                grid [] [ spacing 4 ] <|
                    [ [ keyword "if", render p, keyword "then" ]
                    , [ tab, render x ]
                    , [ keyword "else" ]
                    , [ tab, render y ]
                    ]

            ( Assignment, [ name, expression ] ) ->
                grid [] [ spacing 4 ] <|
                    [ [ render name, text "=" ]
                    , [ tab, render expression ]
                    ]

            ( Id s, _ ) ->
                text s

            ( List, [] ) ->
                text "[]"

            ( List, e :: es ) ->
                row [] <|
                    [ row [] [ text "[", render e ]
                    , row [] <| List.map (\x -> row [] [ text ",", render x ]) es
                    , text "]"
                    ]

            ( Hole, _ ) ->
                el [ Background.color (rgb255 255 110 110) ] (text "_")

            _ ->
                Debug.todo <| "Missing branch in render function: " ++ Debug.toString ( label.value, children )


view : Model -> Html msg
view model =
    Element.layout
        [ Font.family [ Font.monospace ]
        , Font.color (rgb255 0 0 0)
        , Element.paddingXY 5 5
        ]
    <|
        (model.zipper
            |> Zipper.root
            |> Zipper.tree
            |> render
        )



-- MODEL


type alias Model =
    { zipper : Zipper Label }


type alias Label =
    { value : Node, selected : Bool }


type Node
    = If
    | Id String
    | Assignment
    | List
    | Hole


t =
    node Assignment
        [ leaf <| Id "foo"
        , node If
            [ leaf <| Id "x"
            , leaf <| Id "y"
            , node List [ leaf <| Id "z", leaf <| Id "l" ]

            --, node List []
            ]
        ]



--Tree.tree (Label If False) <| List.map (Tree.singleton << (\x -> Label x False)) [ Id "x", Id "y", Id "z" ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model <| Zipper.fromTree t
    , Cmd.none
    )



--node_ v c =
--    Tree.tree (Label v True) c
--
--


node v =
    Tree.tree (Label v False)


leaf v =
    node v []



--testTree =
--    node 1
--        [ node 2
--            [ node 7 []
--            , node 6 []
--            ]
--        , node 3 []
--        , node 4 []
--        ]
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
