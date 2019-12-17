module Main exposing (..)

import Browser
import Element exposing (Element, alignRight, centerY, column, el, fill, padding, paddingXY, rgb255, row, spacing, text, width)
import Element.Background as Background exposing (color)
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html, div, h1, img)
import Html.Attributes exposing (src)



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
    Element.layout []
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
    Case (Character 'a') [ ( Character 'b', Character 'c' ) ]


renderExpression : Expression -> Element msg
renderExpression expression =
    case expression of
        Character char ->
            text <| "'" ++ String.fromChar char ++ "'"

        Case e patterns ->
            let
                renderBranch ( pattern, expr ) =
                    column []
                        [ row [] [ renderExpression pattern, text " -> " ]
                        , el [ paddingXY 20 0 ] (renderExpression expr)
                        ]
            in
            column []
                [ row [] [ text "case", el [ paddingXY 5 0 ] <| renderExpression e, text "of" ]
                , row [ paddingXY 20 0 ] <| List.map renderBranch patterns
                ]

        _ ->
            Debug.todo ""


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
