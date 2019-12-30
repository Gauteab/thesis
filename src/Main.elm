module Main exposing (..)

import Browser
import Element exposing (Element, alignTop, column, el, paddingXY, rgb255, row, text)
import Element.Background as Background
import Element.Font as Font exposing (Font)
import Html exposing (Html)
import List.Extra as List



---- MODEL ----


type alias Model =
    ConceptNode


exampleQuery =
    [ "branch"
    , "int"
    ]


example =
    List
        [ Integer 1
        , Case (String "avc")
            [ ( String "a"
              , List
                    [ Integer 2
                    , Integer 3
                    , Case (String "xyz")
                        [ ( String "b"
                          , List [ Integer 4, String "c" ]
                          )
                        ]
                    ]
              )
            , ( Integer 5, Integer 6 )
            ]
        ]
        |> expressionToConceptNode 0


init : ( Model, Cmd Msg )
init =
    ( Tuple.second example, Cmd.none )


type alias Query =
    List String


type alias Id =
    Int


type alias ConceptNode =
    { name : String
    , id : Id
    , concept : Concept
    }


type Concept
    = Hole
    | Leaf String
    | Node (List ConceptNode)


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
    <|
        column
            []
            [ renderConcept (runQuery [] exampleQuery model) model
            ]


renderConcept : List Id -> ConceptNode -> Element msg
renderConcept hits conceptNode =
    let
        cEl =
            codeElement (isMultiline conceptNode) (List.member conceptNode.id hits)
    in
    row [] <|
        [ --el [ alignTop ] <| text (String.fromInt conceptNode.id ++ ": ")
          case ( conceptNode.name, conceptNode.concept ) of
            ( "int", Leaf x ) ->
                cEl [ text x ]

            ( "string", Leaf x ) ->
                cEl [ text <| "\"" ++ x ++ "\"" ]

            ( "case", Node (e :: es) ) ->
                cEl
                    [ row [] [ text "case", el [ paddingXY 5 0 ] <| renderConcept hits e, text "of" ]
                    , column [ paddingXY 25 0 ] <| List.map (renderConcept hits) es
                    ]

            ( "list", Node [] ) ->
                cEl []

            ( "list", Node (e :: es) ) ->
                cEl <|
                    row [] [ text "[", renderConcept hits e ]
                        :: List.map (\it -> row [] [ el [ alignTop ] (text ","), renderConcept hits it ]) es
                        ++ [ text "]" ]

            ( "branch", Node [ pattern, expr ] ) ->
                column []
                    [ row [] [ renderConcept hits pattern, text " -> " ]
                    , el [ paddingXY 25 0 ] (renderConcept hits expr)
                    ]

            _ ->
                Debug.todo "render concept"
        ]


isMultiline : ConceptNode -> Bool
isMultiline conceptNode2 =
    case ( conceptNode2.concept, conceptNode2.name ) of
        ( Node _, "case" ) ->
            True

        ( Node list, "list" ) ->
            List.any isMultiline list

        _ ->
            False


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



---- PROGRAM ----


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
        Node xs ->
            List.concatMap (runQuery newFound newQuery) xs

        _ ->
            newFound


queryMatch : String -> String -> Bool
queryMatch query goal =
    query == goal


expressionToConceptNode : Int -> Expression -> ( Int, ConceptNode )
expressionToConceptNode count expression =
    case expression of
        String s ->
            ( count + 1, ConceptNode "string" count <| Leaf s )

        Integer x ->
            ( count + 1, ConceptNode "int" count <| Leaf (String.fromInt x) )

        List xs ->
            let
                f expr ( currentCount, currentConcepts ) =
                    let
                        ( newCount, newConcepts ) =
                            expressionToConceptNode currentCount expr
                    in
                    ( newCount, newConcepts :: currentConcepts )

                ( newCount_, cs ) =
                    List.foldl f ( count + 1, [] ) xs
            in
            ( newCount_, ConceptNode "list" count <| Node (List.reverse cs) )

        Case e patterns ->
            let
                toBranch ( a, b ) ( currentCount, currentConcepts ) =
                    let
                        ( count1, concept1 ) =
                            expressionToConceptNode (currentCount + 1) a

                        ( count2, concept2 ) =
                            expressionToConceptNode count1 b
                    in
                    ( count2, (ConceptNode "branch" currentCount <| Node [ concept1, concept2 ]) :: currentConcepts )

                ( newCount, c ) =
                    expressionToConceptNode (count + 1) e

                ( newCount2, cs ) =
                    List.foldl toBranch ( newCount, [] ) patterns
            in
            ( newCount2, ConceptNode "case" count <| Node (c :: List.reverse cs) )

        _ ->
            Debug.todo "Unhandled expression in expressionToConceptNode"


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
