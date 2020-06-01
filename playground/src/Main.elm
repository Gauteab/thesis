module Main exposing (..)

import Browser
import Browser.Events
import Dict
import Element exposing (Element, Length, alignTop, column, el, px, rgb255, row, spacing, text)
import Element.Background as Background
import Element.Font as Font exposing (Font)
import Elm.Parser
import Elm.Processing as Processing
import Elm.RawFile exposing (RawFile)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node as N exposing (Node(..))
import Html exposing (Html)
import Json.Decode
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Tree as Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)
import View



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { zipper : Zipper Label
    , inputBuffer : String
    }


type alias Label =
    { value : Node, selected : Bool }


type Node
    = If
    | Id String
    | Assignment
    | List
    | Hole
    | Module


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (Zipper.fromTree (parse testFile)) ""
    , Cmd.none
    )


node v =
    Tree.tree (Label v False)


leaf v =
    node v []


testFile =
    """
module Test exposing (..)
food = if x then y else [z,l]
"""


processRawFile : RawFile -> Tree Label
processRawFile rawFile =
    let
        file : File
        file =
            Processing.process Processing.init rawFile

        processExpression : Expression -> Tree Label
        processExpression e =
            case e of
                ListExpr es ->
                    node List (List.map (processExpression << N.value) es)

                IfBlock e1 e2 e3 ->
                    node If (List.map (processExpression << N.value) [ e1, e2, e3 ])

                FunctionOrValue _ s ->
                    leaf (Id s)

                _ ->
                    Debug.todo (Debug.toString e)

        processFile : File -> List (Tree Label)
        processFile =
            .declarations >> List.map (N.value >> processDecoration)

        processDecoration : Declaration -> Tree Label
        processDecoration declaration =
            case declaration of
                FunctionDeclaration function ->
                    let
                        implementation =
                            N.value function.declaration
                    in
                    node Assignment
                        [ leaf (Id (N.value implementation.name))
                        , processExpression (N.value implementation.expression)
                        ]

                _ ->
                    Debug.todo ""
    in
    node Module <| processFile file


parse : String -> Tree Label
parse input =
    case Elm.Parser.parse input of
        Err e ->
            Debug.todo ""

        Ok v ->
            processRawFile v



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
                keyToActionMap =
                    Dict.fromList
                        [ ( "j", Move Down )
                        , ( "k", Move Up )
                        , ( "h", Move Left )
                        , ( "l", Move Right )
                        , ( "d", Delete )
                        , ( "ri", Replace If )
                        , ( "Escape", ClearInputBuffer )
                        ]

                newInput : String
                newInput =
                    case Debug.log "key" keyboardEvent.key of
                        Nothing ->
                            model.inputBuffer

                        Just "Escape" ->
                            ""

                        Just s ->
                            if String.length s == 1 then
                                model.inputBuffer ++ s

                            else
                                model.inputBuffer

                newModel : Model
                newModel =
                    case Dict.get newInput keyToActionMap of
                        Just a ->
                            doAction model a
                                |> (\r -> { r | inputBuffer = "" })

                        Nothing ->
                            { model | inputBuffer = newInput }
            in
            ( newModel, Cmd.none )


type Direction
    = Down
    | Up
    | Right
    | Left


type Action
    = Move Direction
    | ClearInputBuffer
    | Delete
    | Replace Node


doMove : Direction -> Zipper Label -> Zipper Label
doMove direction zipper =
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


mapZipper f model =
    { model | zipper = f model.zipper }


doAction : Model -> Action -> Model
doAction model action =
    case action of
        Move direction ->
            mapZipper (doMove direction) model

        Delete ->
            mapZipper doDelete model

        ClearInputBuffer ->
            { model | inputBuffer = "" }

        Replace If ->
            mapZipper (Zipper.mapTree <| always <| node If [ leaf Hole, leaf Hole, leaf Hole ]) model

        Replace _ ->
            Debug.todo ""


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
    in
    View.highlighted label.selected <|
        case ( label.value, children ) of
            ( If, [ p, x, y ] ) ->
                View.iff render p x y

            ( If, _ ) ->
                Debug.todo <| "Invalid if: " ++ Debug.toString label.value

            ( Assignment, [ name, expression ] ) ->
                View.assignment render name expression

            ( Assignment, _ ) ->
                Debug.todo <| "Invalid assignment: " ++ Debug.toString label.value

            ( Id s, _ ) ->
                View.identifier s

            ( List, es ) ->
                View.list render es

            ( Hole, _ ) ->
                View.hole

            ( Module, declarations ) ->
                View.module_ render declarations


view : Model -> Html msg
view model =
    View.editorLayout <|
        column [ Element.height Element.fill ]
            [ model.zipper
                |> Zipper.root
                |> Zipper.tree
                |> render
            , el [ Element.alignBottom ] <| text model.inputBuffer
            ]



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
