module Zip exposing (..)

import Browser
import Browser.Events
import Dict
import Element exposing (Element, column, el, text)
import Html exposing (Html)
import Json.Decode
import Keyboard.Event exposing (KeyboardEvent)
import View


type alias Info =
    Bool


type alias Tree =
    Assignment


type I a
    = I Info a


type alias Expression =
    I Expression_


type Expression_
    = Int Int
    | Plus Expression Expression
    | If Expression Expression Expression
    | List (List Expression)


type alias Name =
    I String


type alias Assignment =
    I Assignment_


type Assignment_
    = Assignment Name Arguments Expression


type alias Arguments =
    I (List Name)


type alias Zip a =
    { before : List a
    , after : List a
    }


type alias ExpressionContext =
    I ExpressionContext_


type ExpressionContext_
    = Assignment3 Name Arguments
    | Plus1 ExpressionContext Expression
    | Plus2 Expression ExpressionContext
    | If1 ExpressionContext Expression Expression
    | If2 Expression ExpressionContext Expression
    | If3 Expression Expression ExpressionContext
    | ListContext (Zip Expression) ExpressionContext


type alias ArgumentContext =
    I ArgumentContext_


type ArgumentContext_
    = Assignment2 Name Expression


type alias StringContext =
    I StringContext_


type StringContext_
    = Assignment1 Arguments Expression
    | Arguments (Zip Name) ArgumentContext


type Zipper
    = S Name StringContext
    | E Expression ExpressionContext
    | A Assignment
    | L Arguments ArgumentContext


modifyInfo : (Info -> Info) -> Zipper -> Zipper
modifyInfo f zipper =
    case zipper of
        E (I i e) c ->
            E (I (f i) e) c

        S (I i name) c ->
            S (I (f i) name) c

        A (I i e) ->
            A (I (f i) e)

        L (I i a) c ->
            L (I (f i) a) c


modifyS : (String -> String) -> Zipper -> Zipper
modifyS function zipper =
    case zipper of
        S (I i e) c ->
            S (I i (function e)) c

        _ ->
            Debug.todo ""


modifyE : (Expression -> Expression) -> Zipper -> Zipper
modifyE function zipper =
    case zipper of
        E e c ->
            E (function e) c

        _ ->
            Debug.todo ""


root : Zipper -> Zipper
root zipper =
    case up zipper of
        Just a ->
            root a

        Nothing ->
            zipper


tree : Zipper -> Tree
tree zipper =
    case zipper of
        A assignment ->
            assignment

        _ ->
            Debug.todo (Debug.toString zipper)


left : Zipper -> Maybe Zipper
left zipper =
    case zipper of
        E _ (I i (Plus1 _ _)) ->
            Nothing

        E e2 (I i (Plus2 e1 c)) ->
            Just <| E e1 (I i (Plus1 c e2))

        E _ (I i (If1 _ _ _)) ->
            Nothing

        E e2 (I i (If2 e1 c e3)) ->
            Just <| E e1 (I i (If1 c e2 e3))

        E e3 (I i (If3 e1 e2 c)) ->
            Just <| E e2 (I i (If2 e1 c e3))

        E e2 (I i (Assignment3 s1 a)) ->
            Just <| S s1 (I i (Assignment1 a e2))

        E e1 (I i (ListContext { before, after } c)) ->
            case before of
                [] ->
                    Nothing

                x :: xs ->
                    Just <| E x (I i (ListContext { before = xs, after = e1 :: after } c))

        S s1 (I i (Arguments { before, after } c)) ->
            case before of
                [] ->
                    Nothing

                x :: xs ->
                    Just <| S x (I i (Arguments { before = xs, after = s1 :: after } c))

        S _ (I i (Assignment1 _ _)) ->
            Nothing

        A _ ->
            Nothing

        L a (I i (Assignment2 s e)) ->
            Just <| S s (I i (Assignment1 a e))


right : Zipper -> Maybe Zipper
right zipper =
    case zipper of
        E e1 (I i (Plus1 c e2)) ->
            Just <| E e2 (I i (Plus2 e1 c))

        E _ (I i (Plus2 _ _)) ->
            Nothing

        E e1 (I i (If1 c e2 e3)) ->
            Just <| E e2 (I i (If2 e1 c e3))

        E e2 (I i (If2 e1 c e3)) ->
            Just <| E e3 (I i (If3 e1 e2 c))

        E _ (I i (If3 _ _ _)) ->
            Nothing

        E _ (I i (Assignment3 _ _)) ->
            Nothing

        E e1 (I i (ListContext { before, after } c)) ->
            case after of
                [] ->
                    Nothing

                x :: xs ->
                    Just <| E x (I i (ListContext { before = e1 :: before, after = xs } c))

        L a (I i (Assignment2 s1 e1)) ->
            Just <| E e1 (info <| Assignment3 s1 a)

        S s1 (I i (Arguments { before, after } c)) ->
            case after of
                [] ->
                    --Just <| E e1 (Assignment3 s2 (List.reverse (s1 :: as1)))
                    Nothing

                x :: xs ->
                    Just <| S x (I i (Arguments { before = s1 :: before, after = xs } c))

        S s (I i (Assignment1 ar e)) ->
            Just <| L ar (I i (Assignment2 s e))

        A _ ->
            Nothing


up : Zipper -> Maybe Zipper
up zipper =
    case zipper of
        E e1 (I i (Assignment3 s1 a)) ->
            Just <| A (I i (Assignment s1 a e1))

        E e1 (I i (Plus1 c e2)) ->
            Just <| E (I i (Plus e1 e2)) c

        E e2 (I i (Plus2 e1 c)) ->
            Just <| E (I i (Plus e1 e2)) c

        E e1 (I i (If1 c e2 e3)) ->
            Just <| E (I i (If e1 e2 e3)) c

        E e2 (I i (If2 e1 c e3)) ->
            Just <| E (I i (If e1 e2 e3)) c

        E e3 (I i (If3 e1 e2 c)) ->
            Just <| E (I i (If e1 e2 e3)) c

        E e1 (I i (ListContext { before, after } c)) ->
            Just <| E (I i (List (List.reverse before ++ e1 :: after))) c

        S s1 (I i (Assignment1 arguments e2)) ->
            Just <| A (I i (Assignment s1 arguments e2))

        S s1 (I i (Arguments { before, after } c)) ->
            Just <| L (I i (List.reverse before ++ (s1 :: after))) c

        L a (I i (Assignment2 s1 e1)) ->
            Just <| A (I i (Assignment s1 a e1))

        A _ ->
            Nothing


down : Zipper -> Maybe Zipper
down zipper =
    case zipper of
        E (I i (Int _)) _ ->
            Nothing

        E (I i (Plus e1 e2)) c ->
            Just <| E e1 (I i (Plus1 c e2))

        E (I i (If e1 e2 e3)) c ->
            Just <| E e1 (I i (If1 c e2 e3))

        E (I i (List es)) c ->
            case es of
                [] ->
                    Nothing

                x :: xs ->
                    Just <| E x <| I i <| ListContext (Zip [] xs) c

        A (I i (Assignment s a e2)) ->
            Just <| S s (I i (Assignment1 a e2))

        L (I i a) c ->
            case a of
                [] ->
                    Nothing

                x :: xs ->
                    Just <| S x (I i (Arguments { before = [], after = xs } c))

        S _ _ ->
            Nothing


main_ =
    z
        |> down
        |> Maybe.andThen right
        |> Maybe.andThen right
        |> Maybe.andThen up


z =
    A <|
        info <|
            Assignment (info "foo")
                (info [ info "x", info "y" ])
                (info <|
                    If (info <| List [ info <| Int 7, info <| Int 8 ])
                        (info <| Int 5)
                        (info <| Int 3)
                )


info =
    I False



--newListZipper list =
--    Zip [] list
--
--
--listLeft : Zip a -> Maybe (Zip a)
--listLeft { before, after } =
--    case before of
--        [] ->
--            Nothing
--
--        x :: xs ->
--            Just <| Zip xs (x :: after)
--
--
--listRight : Zip a -> Maybe (Zip a)
--listRight { before, after } =
--    case after of
--        [] ->
--            Nothing
--
--        x :: xs ->
--            Just <| Zip (x :: before) xs
-- Application


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { zipper : Zipper
    , inputBuffer : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model z ""
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


doMove direction zipper =
    let
        --moveZipper : Zipper label -> Maybe (Zipper label)
        moveZipper =
            case direction of
                Down ->
                    down

                Up ->
                    up

                Right ->
                    right

                Left ->
                    left
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

        Replace TargetIf ->
            let
                new =
                    Debug.todo ""
            in
            mapZipper (modifyE <| always <| new) model

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


doDelete : Zipper -> Zipper
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


setSelected : Bool -> Zipper -> Zipper
setSelected bool =
    modifyInfo (always bool)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onKeyDown <| Json.Decode.map HandleKeyboardEvent Keyboard.Event.decodeKeyboardEvent



-- VIEW


renderAssignment : Assignment -> Element msg
renderAssignment (I selected (Assignment name args expression)) =
    let
        renderName : Name -> Element msg
        renderName (I i s) =
            View.highlighted i <|
                text s

        renderArguments (I i arguments) =
            View.highlighted i <|
                Element.row [ Element.spacing 4 ] (List.map renderName arguments)

        renderExpression : Expression -> Element msg
        renderExpression (I i e) =
            View.highlighted i <|
                case e of
                    Int int ->
                        text (String.fromInt int)

                    Plus e1 e2 ->
                        Debug.todo ""

                    If p x y ->
                        View.iff renderExpression p x y

                    List es ->
                        View.list renderExpression es
    in
    View.highlighted selected <|
        --View.assignment renderExpression (iget name) (List.map iget (iget args)) expression
        View.grid []
            [ Element.spacing 4 ]
            [ [ renderName name, renderArguments args, text "=" ]
            , [ View.tab, renderExpression expression ]
            ]


iget (I _ a) =
    a


view : Model -> Html msg
view model =
    View.editorLayout <|
        column [ Element.height Element.fill ]
            [ model.zipper
                |> root
                |> tree
                |> renderAssignment
            , text <| Debug.toString model.zipper
            , el [ Element.alignBottom ] <| text model.inputBuffer
            ]
