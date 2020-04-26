module Main exposing (..)

import Browser
import Browser.Events
import Element exposing (Element, alignBottom, alignTop, column, el, rgb, rgb255, row, spacing, text)
import Element.Background as Background
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


type Action
    = Down
    | Up
    | Right
    | Left


doAction : Model -> Action -> Model
doAction model action =
    let
        moveZipper : Zipper label -> Maybe (Zipper label)
        moveZipper =
            case action of
                Down ->
                    Zipper.forward

                Up ->
                    Zipper.parent

                Right ->
                    Zipper.nextSibling

                Left ->
                    Zipper.previousSibling
    in
    model.zipper
        |> setSelected False
        |> moveZipper
        |> Maybe.map (setSelected True)
        |> Maybe.withDefault model.zipper
        |> Model


keyToAction : String -> Maybe Action
keyToAction key =
    case key of
        "l" ->
            Just Right

        "h" ->
            Just Left

        "k" ->
            Just Up

        "j" ->
            Just Down

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


view : Model -> Html msg
view model =
    Element.layout [] <|
        render (Zipper.root model.zipper |> Zipper.tree)



-- MODEL


type alias Model =
    { zipper : Zipper Label }


type alias Label =
    { value : Int, selected : Bool }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model z
    , Cmd.none
    )


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



--|> Zipper.mapLabel (\l -> { l | selected = True })
--|> Zipper.forward
--|> Maybe.map (Zipper.mapLabel (\l -> { l | selected = True }))
--|> Maybe.andThen Zipper.nextSibling
--|> (Maybe.map << Zipper.mapLabel) (\l -> { l | selected = True })
--|> Maybe.withDefault (Zipper.fromTree testTree)
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
