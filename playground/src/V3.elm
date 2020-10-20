module V3 exposing (..)

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
    = I Int a


type Expression
    = Int Int
    | If (Node Expression) (Node Expression) (Node Expression)
    | List (List (Node Expression))


type alias Model =
    { zipper : Zipper Int
    , ast : Node Expression
    , inputBuffer : String
    , selected : List Int
    }


ast =
    I 0 <|
        If (I 1 <| List [ I 2 <| Int 7, I 3 <| Int 8 ])
            (I 4 <| Int 5)
            (I 5 <| Int 3)


expressionToRose : Node Expression -> Tree Int
expressionToRose (I id expression) =
    case expression of
        Int int ->
            tree id []

        If p x y ->
            tree id (List.map expressionToRose [ p, x, y ])

        List nodes ->
            tree id (List.map expressionToRose nodes)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { zipper = Zipper.fromTree (expressionToRose ast)
      , ast = ast
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
            [ Zipper.label newZipper ]
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
            mapZipper doDelete model

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


doDelete zipper =
    Debug.todo ""



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
            [ renderExpression model.selected model.ast
            , Element.paragraph [] [ text <| Debug.toString model.zipper ]
            , el [ Element.alignBottom ] <| text model.inputBuffer
            ]
