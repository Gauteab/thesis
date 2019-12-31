module Main exposing (..)

import Browser
import Element exposing (Element, alignTop, column, el, paddingXY, rgb255, row, spacing, text)
import Element.Background as Background
import Element.Border as Border
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


type alias QueryResult =
    List ( Index, Id )


type alias Id =
    Int


type alias Index =
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
            [ renderConcept (runQuery exampleQuery model) model
            ]


renderConcept : QueryResult -> ConceptNode -> Element msg
renderConcept queryResult conceptNode =
    let
        maybeHit =
            queryResult
                |> List.find (Tuple.second >> (==) conceptNode.id)
                |> Maybe.map Tuple.first

        cEl =
            codeElement (isMultiline conceptNode) maybeHit
    in
    case ( conceptNode.name, conceptNode.concept ) of
        ( "int", Leaf x ) ->
            cEl [ text x ]

        ( "string", Leaf x ) ->
            cEl [ text <| "\"" ++ x ++ "\"" ]

        ( "case", Node (e :: es) ) ->
            cEl
                [ row [] [ text "case", el [ paddingXY 5 0 ] <| renderConcept queryResult e, text "of" ]
                , column [ paddingXY 25 0 ] <| List.map (renderConcept queryResult) es
                ]

        ( "list", Node [] ) ->
            cEl []

        ( "list", Node (e :: es) ) ->
            cEl <|
                row [] [ text "[", renderConcept queryResult e ]
                    :: List.map (\it -> row [] [ el [ alignTop ] (text ","), renderConcept queryResult it ]) es
                    ++ [ text "]" ]

        ( "branch", Node [ pattern, expr ] ) ->
            column []
                [ row [] [ renderConcept queryResult pattern, text " -> " ]
                , el [ paddingXY 25 0 ] (renderConcept queryResult expr)
                ]

        _ ->
            Debug.todo "render concept"


isMultiline : ConceptNode -> Bool
isMultiline conceptNode2 =
    case ( conceptNode2.concept, conceptNode2.name ) of
        ( Node _, "case" ) ->
            True

        ( Node list, "list" ) ->
            List.any isMultiline list

        _ ->
            False


codeElement : Bool -> Maybe Index -> List (Element msg) -> Element msg
codeElement multiline maybeHit =
    let
        indicator : Id -> Element msg
        indicator id =
            el
                [ Border.solid
                , Border.width 1
                , alignTop
                , Background.color (rgb255 240 225 180)
                ]
            <|
                text (String.fromInt id)

        highlightColor =
            Background.color (rgb255 55 210 185)
    in
    case ( multiline, maybeHit ) of
        ( False, Nothing ) ->
            row []

        ( False, Just index ) ->
            \elements -> row [ spacing 4 ] [ indicator index, row [ highlightColor ] elements ]

        ( True, Nothing ) ->
            column []

        ( True, Just index ) ->
            \elements -> row [ spacing 4 ] [ indicator index, column [ highlightColor ] elements ]



---- PROGRAM ----


runQuery : Query -> ConceptNode -> QueryResult
runQuery q cn =
    let
        go : List Id -> Query -> ConceptNode -> List Id
        go found query conceptNode =
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
                    List.concatMap (go newFound newQuery) xs

                _ ->
                    newFound
    in
    go [] q cn
        |> List.indexedMap Tuple.pair


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
                    List.foldr f ( count + 1, [] ) xs
            in
            ( newCount_, ConceptNode "list" count <| Node cs )

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
                    List.foldr toBranch ( newCount, [] ) patterns
            in
            ( newCount2, ConceptNode "case" count <| Node (c :: cs) )

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
