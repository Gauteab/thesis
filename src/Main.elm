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
    [ "case"
    , "int"
    ]


example =
    EList
        [ EInteger 1
        , EList [ EInteger 6 ]
        , ECase (EString "avc")
            [ ( EString "a"
              , EList
                    [ EInteger 2
                    , EInteger 3
                    , ECase (EString "xyz")
                        [ ( EString "b"
                          , EList [ EInteger 4, EString "c" ]
                          )
                        ]
                    ]
              )
            , ( EInteger 5, EInteger 6 )
            ]
        ]
        |> expressionToConceptNode 0


init : ( Model, Cmd Msg )
init =
    ( Tuple.second example, Cmd.none )


type alias Query =
    List String


type alias QueryResult =
    List Id


type alias QueryResultIndexed =
    List ( Index, Id )


type alias Id =
    Int


type alias Index =
    Int


type alias ConceptNode =
    { id : Id
    , concept : Concept
    }


type LeafName
    = String
    | Integer


type NodeName
    = List
    | Branch
    | Case


type Concept
    = Hole
    | Leaf LeafName String
    | Node NodeName (List ConceptNode)


type Expression
    = ECharacter Char
    | EString String
    | EInteger Int
    | EFloat Float
    | EVariable (List String)
    | EList (List Expression)
    | ETuple (List Expression)
    | EAccess Expression (List String)
    | EAccessFunction String
    | ERecord (List ( String, Expression ))
    | ERecordUpdate String (List ( String, Expression ))
    | EIf Expression Expression Expression
    | ELet (List ( Expression, Expression )) Expression
    | ECase Expression (List ( Expression, Expression ))
    | ELambda (List Expression) Expression
    | EApplication Expression Expression
    | EBinOp Expression Expression Expression



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
            [ renderConcept
                (Debug.log "" <| List.indexedMap Tuple.pair <| runQuery exampleQuery model)
                (Debug.log "model" model)
            ]


renderConcept : QueryResultIndexed -> ConceptNode -> Element msg
renderConcept queryResult conceptNode =
    let
        maybeHit =
            queryResult
                |> List.find (Tuple.second >> (==) conceptNode.id)
                |> Maybe.map Tuple.first

        cEl =
            codeElement (isMultiline conceptNode) maybeHit
    in
    row []
        [ -- indicator conceptNode.id
          case conceptNode.concept of
            Leaf Integer string ->
                cEl [ text string ]

            Leaf String string ->
                cEl [ text <| "\"" ++ string ++ "\"" ]

            Node Case (pattern :: branches) ->
                cEl
                    [ row [] [ text "case", el [ paddingXY 5 0 ] <| renderConcept queryResult pattern, text "of" ]
                    , column [ paddingXY 25 0 ] <| List.map (renderConcept queryResult) branches
                    ]

            Node List [] ->
                cEl []

            Node List (e :: es) ->
                cEl <|
                    row [] [ text "[", renderConcept queryResult e ]
                        :: List.map (\it -> row [] [ el [ alignTop ] (text ","), renderConcept queryResult it ]) es
                        ++ [ text "]" ]

            Node Branch [ pattern, expr ] ->
                cEl <|
                    [ row [] [ renderConcept queryResult pattern, text " -> " ]
                    , el [ paddingXY 25 0 ] (renderConcept queryResult expr)
                    ]

            _ ->
                Debug.todo "render concept"
        ]


isMultiline : ConceptNode -> Bool
isMultiline conceptNode2 =
    case conceptNode2.concept of
        Node Case _ ->
            True

        Node Branch _ ->
            True

        Node List list ->
            List.any isMultiline list

        Hole ->
            False

        Leaf _ _ ->
            False


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


codeElement : Bool -> Maybe Index -> (List (Element msg) -> Element msg)
codeElement multiline maybeHit =
    let
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


getChildren : ConceptNode -> List ConceptNode
getChildren node =
    case node.concept of
        Node _ xs ->
            xs

        _ ->
            []


runQuery : Query -> ConceptNode -> QueryResult
runQuery originalQuery conceptNode =
    let
        go query node =
            let
                ( queryHead, queryTail ) =
                    List.uncons query |> Maybe.withDefault ( "", [] )

                ( newQuery, newFound ) =
                    case ( queryMatch queryHead node, List.isEmpty queryTail ) of
                        ( True, True ) ->
                            ( originalQuery, Just node.id )

                        ( True, False ) ->
                            ( queryTail, Nothing )

                        _ ->
                            ( query, Nothing )
            in
            case newFound of
                Just found ->
                    found :: List.concatMap (go newQuery) (getChildren node)

                Nothing ->
                    List.concatMap (go newQuery) (getChildren node)
    in
    go originalQuery conceptNode


queryMatch : String -> ConceptNode -> Bool
queryMatch query goal =
    case ( query, goal.concept ) of
        ( "hole", Hole ) ->
            True

        ( "string", Leaf String _ ) ->
            True

        ( "int", Leaf Integer _ ) ->
            True

        ( "case", Node Case _ ) ->
            True

        ( "branch", Node Branch _ ) ->
            True

        ( "list", Node List _ ) ->
            True

        _ ->
            False


delete : QueryResult -> ConceptNode -> ConceptNode
delete queryResult conceptNode =
    Debug.todo ""


expressionToConceptNode : Int -> Expression -> ( Int, ConceptNode )
expressionToConceptNode count expression =
    case expression of
        EInteger x ->
            ( count + 1, ConceptNode count <| Leaf Integer (String.fromInt x) )

        EString s ->
            ( count + 1, ConceptNode count <| Leaf String s )

        EList xs ->
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
            ( newCount_, ConceptNode count <| Node List cs )

        ECase e patterns ->
            let
                toBranch ( a, b ) ( currentCount, currentConcepts ) =
                    let
                        ( count1, concept1 ) =
                            expressionToConceptNode (currentCount + 1) a

                        ( count2, concept2 ) =
                            expressionToConceptNode count1 b
                    in
                    ( count2, (ConceptNode currentCount <| Node Branch [ concept1, concept2 ]) :: currentConcepts )

                ( newCount, c ) =
                    expressionToConceptNode (count + 1) e

                ( newCount2, cs ) =
                    List.foldr toBranch ( newCount, [] ) patterns
            in
            ( newCount2, ConceptNode count <| Node Case (c :: cs) )

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
