module Main exposing (..)

import Browser
import Element exposing (Color, Element, alignTop, column, el, fill, paddingXY, px, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font exposing (Font)
import Element.Input as Input exposing (labelHidden)
import Html exposing (Html)
import List.Extra as List
import Parser as P exposing ((|.), (|=), Parser, Trailing(..), backtrackable, keyword, oneOf, spaces, succeed, symbol, token)



---- MODEL ----


type alias Model =
    { maxId : Int
    , conceptNode : ConceptNode
    , queryResult : QueryResult
    , inputText : String
    }


init : ( Model, Cmd Msg )
init =
    let
        ( id, node ) =
            expressionToConceptNode 0 <|
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
    in
    ( { maxId = id
      , conceptNode = node
      , queryResult = []
      , inputText = ""
      }
    , Cmd.none
    )


type alias Query =
    ( Name, List Name )


type alias QueryResult =
    List Id


type alias QueryResultIndexed =
    List ( Index, Id )


type alias Id =
    Int


type alias Index =
    Int


type Action
    = Delete
    | NoAction


type Command
    = Command Query Action


command : Parser Command
command =
    P.succeed Command
        |= parseQuery
        |= oneOf
            [ backtrackable <| succeed identity |. spaces |= action |. symbol " "
            , P.succeed NoAction
            ]


action : Parser Action
action =
    P.map (always Delete) <| oneOf [ keyword "delete", keyword "d" ]


parseQuery : Parser Query
parseQuery =
    let
        toQuery names =
            List.unconsLast names
                |> Maybe.map P.succeed
                |> Maybe.withDefault (P.problem "empty query")
    in
    P.sequence { start = "", separator = ".", end = "", spaces = spaces, item = parseName, trailing = Forbidden }
        |> P.andThen toQuery


fromToken : String -> a -> Parser a
fromToken string a =
    P.map (always a) <| token string


parseName : Parser Name
parseName =
    oneOf
        [ leafName |> P.map LeafName
        , nodeName |> P.map NodeName
        ]


leafName : Parser LeafName
leafName =
    oneOf
        [ fromToken "string" String
        , fromToken "int" Integer
        ]


nodeName : Parser NodeName
nodeName =
    oneOf
        [ fromToken "list" List
        , fromToken "branch" Branch
        , fromToken "case" Case
        ]


type alias ConceptNode =
    { id : Id
    , concept : Concept
    }


type LeafName
    = String
    | Integer


type Name
    = LeafName LeafName
    | NodeName NodeName


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
    | TextInput String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        TextInput string ->
            let
                ( act, queryResult ) =
                    P.run command string
                        |> Result.map (\(Command q a) -> ( a, runQuery q model.conceptNode ))
                        |> Result.withDefault ( NoAction, [] )
            in
            case act of
                NoAction ->
                    ( { model
                        | queryResult = queryResult
                        , inputText = string
                      }
                    , Cmd.none
                    )

                Delete ->
                    ( { model
                        | queryResult = []
                        , inputText = ""
                        , conceptNode = delete queryResult model.conceptNode
                      }
                    , Cmd.none
                    )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        hitsIndexed =
            List.indexedMap Tuple.pair model.queryResult
    in
    Element.layout
        [ Font.family [ Font.monospace ]
        , paddingXY 5 5
        ]
    <|
        column
            [ spacing 50 ]
            [ renderConcept hitsIndexed (Debug.log "model" model).conceptNode
            , Input.text []
                { onChange = TextInput
                , text = model.inputText
                , placeholder = Nothing
                , label = labelHidden "command input field"
                }
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
                cEl [ text "[]" ]

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

            Hole ->
                cEl <|
                    [ el
                        [ Background.color (rgb255 255 110 110) ]
                      <|
                        text (String.fromInt conceptNode.id)
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
runQuery ( name, query ) node =
    let
        ( newQuery, maybeFound ) =
            case query of
                [] ->
                    if match name node.concept then
                        ( [], Just node.id )

                    else
                        ( [], Nothing )

                queryHead :: queryTail ->
                    if match queryHead node.concept then
                        ( queryTail, Nothing )

                    else
                        ( query, Nothing )
    in
    case maybeFound of
        Just found ->
            found :: List.concatMap (runQuery ( name, newQuery )) (getChildren node)

        Nothing ->
            List.concatMap (runQuery ( name, newQuery )) (getChildren node)


match : Name -> Concept -> Bool
match name concept =
    case ( name, concept ) of
        ( LeafName n1, Leaf n2 _ ) ->
            n1 == n2

        ( NodeName n1, Node n2 _ ) ->
            n1 == n2

        _ ->
            False


delete : QueryResult -> ConceptNode -> ConceptNode
delete queryResult conceptNode =
    { conceptNode
        | concept =
            if List.member conceptNode.id queryResult then
                Hole

            else
                case conceptNode.concept of
                    Node List list ->
                        list
                            |> List.filterNot (\it -> List.member it.id queryResult)
                            |> List.map (delete queryResult)
                            |> Node List

                    Node name children ->
                        Node name <| List.map (delete queryResult) children

                    otherwise ->
                        otherwise
    }


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
