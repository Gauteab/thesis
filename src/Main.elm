module Main exposing (..)

import Browser
import Element exposing (Element, alignRight, alignTop, centerY, column, el, fill, padding, paddingXY, rgb255, row, spacing, text, width)
import Element.Background as Background exposing (color)
import Element.Border as Border
import Element.Font as Font exposing (Font)
import Html exposing (Html, div, h1, img)
import Html.Attributes exposing (src)
import List.Extra as List



---- MODEL ----


type alias Model =
    Expression


init : ( Model, Cmd Msg )
init =
    ( example, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    Element.layout
        [ Font.family [ Font.monospace ]
        , paddingXY 5 5
        ]
        (renderExpression [ "case", "list", "int" ] model)



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }


example : Expression
example =
    Case (String "abc") [ ( Character 'a', List [ Integer 1, Integer 2, Case (String "xyz") [ ( Character 'b', List [ Integer 2, Character 'c' ] ) ] ] ), ( Integer 1, Integer 2 ) ]


elementInfo : String -> Expression -> { name : String, multiline : Bool }
elementInfo query expression =
    case expression of
        Case _ _ ->
            { name = "case", multiline = True }

        List xs ->
            { name = "list", multiline = List.any (elementInfo query >> .multiline) xs }

        Integer it ->
            { name = "int", multiline = False }

        _ ->
            { name = "", multiline = False }


queryMatch : String -> String -> Bool
queryMatch query goal =
    query == goal


type alias Query =
    List String


renderExpression : Query -> Expression -> Element msg
renderExpression query expression =
    let
        ( q1, rest ) =
            List.uncons query |> Maybe.withDefault ( "", [] )

        { name, multiline } =
            elementInfo q1 expression

        ( newQuery, highlight ) =
            case ( queryMatch q1 name, List.isEmpty rest ) of
                ( True, True ) ->
                    ( [], True )

                ( True, False ) ->
                    ( rest, False )

                _ ->
                    ( query, False )

        indent =
            paddingXY 25 0

        codeElement e =
            let
                style =
                    if highlight then
                        [ Background.color (rgb255 0 0 255) ]

                    else
                        []
            in
            if multiline then
                column style

            else
                row style
    in
    case expression of
        Character char ->
            text <| "'" ++ String.fromChar char ++ "'"

        Case e patterns ->
            let
                renderBranch ( pattern, expr ) =
                    column []
                        [ row [] [ renderExpression newQuery pattern, text " -> " ]
                        , el [ indent ] (renderExpression newQuery expr)
                        ]
            in
            codeElement expression
                [ row [] [ text "case", el [ paddingXY 5 0 ] <| renderExpression newQuery e, text "of" ]
                , column [ indent ] <| List.map renderBranch patterns
                ]

        String s ->
            text <| "\"" ++ s ++ "\""

        Integer int ->
            codeElement expression <| [ text (String.fromInt int) ]

        Float float ->
            text <| String.fromFloat float

        List [] ->
            text "[]"

        List (x :: xs) ->
            codeElement expression <|
                row [] [ text "[", renderExpression newQuery x ]
                    :: List.map (\e -> row [] [ el [ alignTop ] (text ","), renderExpression newQuery e ]) xs
                    ++ [ text "]" ]

        _ ->
            Debug.todo ""



{-
   x =
       case "abc" of
           'a' ->
               [ 1
               , case "xyz" of
                   'b' ->
                       [ 2, 'c' ]
               ]
-}


type Expression
    = Character Char
    | String String
    | Integer Int
    | Float Float
    | Variable (List String)
    | List (List Expression)
    | Tuple (List Expression)
    | Access Expression (List String)
    | AccessFunction String
    | Record (List ( String, Expression ))
    | RecordUpdate String (List ( String, Expression ))
    | If Expression Expression Expression
    | Let (List ( Expression, Expression )) Expression
    | Case Expression (List ( Expression, Expression ))
    | Lambda (List Expression) Expression
    | Application Expression Expression
    | BinOp Expression Expression Expression
