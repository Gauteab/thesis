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
        (renderConcept model)



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
    Debug.todo ""


codeElement2 =
    let
        ( multiline, highlight ) =
            ( True, False )

        style =
            [ Background.color (rgb255 0 0 255) ]
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


renderConcept : ConceptNode -> Element msg
renderConcept conceptNode =
    case conceptNode.concept of
        IntC x ->
            codeElement2 [ text (String.fromInt x) ]

        StringC s ->
            codeElement2 [ text <| "\"" ++ s ++ "\"" ]

        CaseC e es ->
            let
                renderBranch ( pattern, expr ) =
                    column []
                        [ row [] [ renderConcept pattern, text " -> " ]
                        , el [ paddingXY 25 0 ] (renderConcept expr)
                        ]
            in
            codeElement2
                [ row [] [ text "case", el [ paddingXY 5 0 ] <| renderConcept e, text "of" ]
                , column [ paddingXY 25 0 ] <| List.map renderBranch es
                ]

        ListC [] ->
            codeElement2 []

        ListC (x :: xs) ->
            codeElement2 <|
                row [] [ text "[", renderConcept x ]
                    :: List.map (\e -> row [] [ el [ alignTop ] (text ","), renderConcept e ]) xs
                    ++ [ text "]" ]


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

        codeElement_ e =
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
            codeElement_ expression
                [ row [] [ text "case", el [ paddingXY 5 0 ] <| renderExpression newQuery e, text "of" ]
                , column [ indent ] <| List.map renderBranch patterns
                ]

        String s ->
            text <| "\"" ++ s ++ "\""

        Integer x ->
            codeElement_ expression <| [ text (String.fromInt x) ]

        Float float ->
            text <| String.fromFloat float

        List [] ->
            text "[]"

        List (x :: xs) ->
            codeElement_ expression <|
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
