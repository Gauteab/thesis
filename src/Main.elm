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
        (renderExpression model)



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
    Case (String "abc") [ ( Character 'a', List [ Integer 1, Integer 2, Case (String "xyz") [ ( Character 'b', List [ Integer 2, Character 'c' ] ) ] ] ) ]


multiline : Expression -> Bool
multiline expression =
    case expression of
        Case _ _ ->
            True

        List xs ->
            List.any multiline xs

        _ ->
            False


renderExpression : Expression -> Element msg
renderExpression expression =
    let
        indent =
            paddingXY 25 0

        codeElement e =
            if multiline e then
                column

            else
                row
    in
    case expression of
        Character char ->
            text <| "'" ++ String.fromChar char ++ "'"

        Case e patterns ->
            let
                renderBranch ( pattern, expr ) =
                    column []
                        [ row [] [ renderExpression pattern, text " -> " ]
                        , el [ indent ] (renderExpression expr)
                        ]
            in
            column []
                [ row [] [ text "case", el [ paddingXY 5 0 ] <| renderExpression e, text "of" ]
                , row [ indent ] <| List.map renderBranch patterns
                ]

        String s ->
            text <| "\"" ++ s ++ "\""

        Integer int ->
            text <| String.fromInt int

        Float float ->
            text <| String.fromFloat float

        List [] ->
            text "[]"

        List (x :: xs) ->
            codeElement expression [] <|
                row [] [ text "[", renderExpression x ]
                    :: List.map (\e -> row [] [ el [ alignTop ] (text ","), renderExpression e ]) xs
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
