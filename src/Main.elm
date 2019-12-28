module Main exposing (..)

import Browser
import Element exposing (Element, alignRight, alignTop, centerY, column, el, fill, padding, paddingXY, rgb255, row, spacing, text, width)
import Element.Background as Background exposing (color)
import Element.Border as Border
import Element.Font as Font exposing (Font)
import Html exposing (Html, div, h1, img)
import Html.Attributes exposing (src)
import List.Extra as List
import Tuple.Extra as Tuple



---- MODEL ----


type alias Model =
    ConceptNode


init : ( Model, Cmd Msg )
init =
    ( example2, Cmd.none )



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
        (renderConcept (runQuery [] [ "list", "int" ] model) model)



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


example2 =
    case_ 0 (string 1 "abc") [ ( string 2 "a", list 7 [ int 3 1, int 4 2, case_ 5 (string 6 "xyz") [ ( string 8 "b", list 9 [ int 10 2 ] ) ] ] ), ( int 11 1, int 12 2 ) ]


isMultiline : ConceptNode -> Bool
isMultiline conceptNode =
    case conceptNode.concept of
        IntC _ ->
            False

        StringC _ ->
            False

        CaseC _ _ ->
            True

        ListC xs ->
            List.any isMultiline xs


queryMatch : String -> String -> Bool
queryMatch query goal =
    query == goal


type alias Query =
    List String


type alias Id =
    Int


runQuery : List Id -> Query -> ConceptNode -> List Id
runQuery found query conceptNode =
    let
        ( queryHead, queryTail ) =
            List.uncons query |> Maybe.withDefault ( "", [] )

        ( newQuery, newFound ) =
            case ( queryMatch queryHead conceptNode.name, List.isEmpty queryTail ) of
                ( True, True ) ->
                    ( [], conceptNode.id :: found )

                ( True, False ) ->
                    ( queryTail, found )

                _ ->
                    ( query, found )
    in
    case conceptNode.concept of
        CaseC e es ->
            runQuery newFound newQuery e
                ++ List.concat (List.concatMap ((\( x, y ) -> [ x, y ]) << Tuple.map (runQuery newFound newQuery)) es)

        ListC xs ->
            List.concatMap (runQuery newFound newQuery) xs

        _ ->
            newFound


codeElement multiline highlight =
    let
        style =
            [ Background.color (rgb255 55 210 185) ]
    in
    case ( multiline, highlight ) of
        ( False, False ) ->
            row []

        ( False, True ) ->
            row style

        ( True, False ) ->
            column []

        ( True, True ) ->
            column style


renderConcept : List Id -> ConceptNode -> Element msg
renderConcept hits conceptNode =
    let
        cEl =
            codeElement (isMultiline conceptNode) (List.member conceptNode.id hits)
    in
    case conceptNode.concept of
        IntC x ->
            cEl [ text (String.fromInt x) ]

        StringC s ->
            cEl [ text <| "\"" ++ s ++ "\"" ]

        CaseC e es ->
            let
                renderBranch ( pattern, expr ) =
                    column []
                        [ row [] [ renderConcept hits pattern, text " -> " ]
                        , el [ paddingXY 25 0 ] (renderConcept hits expr)
                        ]
            in
            cEl
                [ row [] [ text "case", el [ paddingXY 5 0 ] <| renderConcept hits e, text "of" ]
                , column [ paddingXY 25 0 ] <| List.map renderBranch es
                ]

        ListC [] ->
            cEl []

        ListC (x :: xs) ->
            cEl <|
                row [] [ text "[", renderConcept hits x ]
                    :: List.map (\e -> row [] [ el [ alignTop ] (text ","), renderConcept hits e ]) xs
                    ++ [ text "]" ]


type alias ConceptNode =
    { name : String
    , id : Id
    , concept : Concept
    }


type Concept
    = IntC Int
    | StringC String
    | CaseC ConceptNode (List ( ConceptNode, ConceptNode ))
    | ListC (List ConceptNode)


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


defaultInfo =
    { name = "", selected = False }


case_ : Int -> ConceptNode -> List ( ConceptNode, ConceptNode ) -> ConceptNode
case_ id e branches =
    { name = "case", id = id, concept = CaseC e branches }


string : Int -> String -> ConceptNode
string id e =
    { name = "string", id = id, concept = StringC e }


int : Int -> Int -> ConceptNode
int id e =
    { name = "int", id = id, concept = IntC e }


list : Int -> List ConceptNode -> ConceptNode
list id e =
    { name = "list", id = id, concept = ListC e }
