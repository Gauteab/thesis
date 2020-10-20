module V4 exposing (..)

import Browser
import Browser.Events
import Dict
import Element exposing (Element, el, row, spacing, text)
import Html exposing (Html)
import Json.Decode
import Keyboard.Event exposing (KeyboardEvent)
import Main exposing (leaf)
import Tree exposing (Tree, tree)
import Tree.Zipper as Zipper exposing (Zipper)
import View exposing (grid, tab)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Node a
    = I Int (Maybe a)


type Expression
    = Int Int
    | If (Node Expression) (Node Expression) (Node Expression)
    | List (List (Node Expression))


type alias Model =
    { zipper : Zipper N

    --, ast : Node Expression
    , inputBuffer : String
    , selected : List Int
    }


n i e =
    I i (Just e)


ast =
    n 0 <|
        If (n 1 <| List [ n 2 <| Int 7, n 3 <| Int 8 ])
            (n 4 <| Int 5)
            (n 5 <| Int 3)


type Kind
    = Int_ Int
    | If_
    | List_
    | Hole


type alias N =
    { id : Int, kind : Kind }


fromRose : Tree N -> Node Expression
fromRose t =
    let
        { id, kind } =
            Tree.label t

        children =
            Tree.children t
    in
    case ( kind, children ) of
        ( Int_ int, [] ) ->
            n id (Int int)

        ( If_, [ p, x, y ] ) ->
            n id (If (fromRose p) (fromRose x) (fromRose y))

        ( List_, es ) ->
            n id (List (List.map fromRose es))

        ( Hole, _ ) ->
            I id Nothing

        _ ->
            Debug.todo ""


toRose : Node Expression -> Tree N
toRose (I id expression) =
    case expression of
        Just (Int int) ->
            tree (N id (Int_ int)) []

        Just (If p x y) ->
            tree (N id If_) (List.map toRose [ p, x, y ])

        Just (List nodes) ->
            tree (N id List_) (List.map toRose nodes)

        Nothing ->
            tree (N id Hole) []


init : () -> ( Model, Cmd Msg )
init _ =
    ( { zipper = toRose ast |> Zipper.fromTree

      --, ast = ast
      , inputBuffer = ""
      , selected = []
      }
    , Cmd.none
    )



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
                newInput_ =
                    newInput model.inputBuffer keyboardEvent.key

                newModel : Model
                newModel =
                    case Dict.get newInput_ keyToActionMap of
                        Just a ->
                            doAction model a
                                |> (\r -> { r | inputBuffer = "" })

                        Nothing ->
                            { model | inputBuffer = newInput_ }
            in
            ( newModel, Cmd.none )



--doMove : Direction -> Zipper Label -> Zipper Label
--let
--    newZipper =
--        Zipper.replaceTree (node Hole []) zipper
--
--    parent =
--        Zipper.parent newZipper |> Maybe.map Zipper.label |> Maybe.map .value
--
--    zipperFilterChildren : (Tree a -> Bool) -> Zipper a -> Zipper a
--    zipperFilterChildren =
--        Zipper.mapTree << Tree.mapChildren << List.filter
--
--    isHole : Tree Label -> Bool
--    isHole =
--        Tree.label >> .value >> (/=) Hole
--in
--case parent of
--    Just List ->
--        Zipper.parent newZipper
--            |> Maybe.map (zipperFilterChildren isHole)
--            |> Maybe.map (setSelected True)
--            |> Maybe.withDefault newZipper
--
--    _ ->
--        newZipper
--setSelected : Bool -> Zipper -> Zipper
--setSelected model=
--    {model |selected = }
-- SUBSCRIPTIONS


doMove : Direction -> Model -> Model
doMove direction model =
    let
        --moveZipper : Zipper label -> Maybe (Zipper label)
        moveZipper =
            case direction of
                Down ->
                    Zipper.firstChild

                Up ->
                    Zipper.parent

                Right ->
                    Zipper.nextSibling

                Left ->
                    Zipper.previousSibling

        newZipper =
            model.zipper
                --|> setSelected False
                |> moveZipper
                --|> Maybe.map (setSelected True)
                |> Maybe.withDefault model.zipper

        selected =
            [ Zipper.label newZipper |> .id ]
    in
    { model | zipper = newZipper, selected = selected }


mapZipper f model =
    { model | zipper = f model.zipper }


doAction : Model -> Action -> Model
doAction model action =
    case action of
        Move direction ->
            doMove direction model

        Delete ->
            doDelete model

        ClearInputBuffer ->
            { model | inputBuffer = "" }

        Replace TargetIf ->
            let
                new =
                    Debug.todo ""
            in
            --mapZipper (modifyE <| always <| new) model
            Debug.todo ""

        Replace _ ->
            Debug.todo ""


type Direction
    = Down
    | Up
    | Right
    | Left


type Action
    = Move Direction
    | ClearInputBuffer
    | Delete
    | Replace Target


type Target
    = TargetIf
    | TargetId
    | TargetAssignment
    | TargetList
    | TargetHole
    | TargetModule


keyToActionMap =
    Dict.fromList
        [ ( "j", Move Down )
        , ( "k", Move Up )
        , ( "h", Move Left )
        , ( "l", Move Right )
        , ( "d", Delete )
        , ( "ri", Replace TargetIf )
        , ( "Escape", ClearInputBuffer )
        ]


newInput : String -> Maybe String -> String
newInput currentInput key =
    case Debug.log "key" key of
        Nothing ->
            currentInput

        Just "Escape" ->
            ""

        Just s ->
            if String.length s == 1 then
                currentInput ++ s

            else
                currentInput



--doDelete : Zipper -> Zipper


parentKind : Zipper N -> Maybe Kind
parentKind zipper =
    Zipper.parent zipper |> Maybe.map (Zipper.label >> .kind)


deletable : Zipper N -> Bool
deletable zipper =
    case parentKind zipper of
        Just List_ ->
            True

        _ ->
            False


doDelete model =
    let
        newZipper =
            if deletable model.zipper then
                Zipper.removeTree model.zipper |> Maybe.withDefault model.zipper

            else
                Zipper.replaceTree (tree (N 9 Hole) []) model.zipper
    in
    { model | zipper = newZipper }



--setSelected : Bool -> Zipper -> Zipper


setSelected model =
    { model | selected = Debug.todo "" }



--SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onKeyDown <| Json.Decode.map HandleKeyboardEvent Keyboard.Event.decodeKeyboardEvent



-- VIEW
--renderAssignment : Assignment -> Element msg


renderExpression : List Int -> Node Expression -> Element msg
renderExpression selected (I i ex) =
    let
        go =
            renderExpression selected

        highlighted =
            View.highlighted (List.member i selected)
    in
    highlighted <|
        case ex of
            Nothing ->
                View.hole

            Just a ->
                case a of
                    Int int ->
                        text (String.fromInt int)

                    If p x y ->
                        grid [] [ spacing 4 ] <|
                            [ [ View.keyword "if", go p, View.keyword "then" ]
                            , [ tab, go x ]
                            , [ View.keyword "else" ]
                            , [ tab, go y ]
                            ]

                    List children ->
                        case children of
                            [] ->
                                text "[]"

                            e :: es ->
                                row [] <|
                                    [ row [] [ text "[", go e ]
                                    , row [] <| List.map (\x -> row [] [ text ",", go x ]) es
                                    , text "]"
                                    ]


view : Model -> Html msg
view model =
    View.editorLayout <|
        Element.column [ Element.height Element.fill ]
            [ renderExpression model.selected (model.zipper |> Zipper.root |> Zipper.tree |> fromRose)
            , Element.paragraph [] [ text <| Debug.toString model.zipper ]
            , el [ Element.alignBottom ] <| text model.inputBuffer
            ]
