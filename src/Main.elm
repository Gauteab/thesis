module Main exposing (..)

import Browser
import Element exposing (Color, Element, alignTop, column, el, fill, paddingXY, px, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font exposing (Font)
import Element.Input as Input exposing (labelHidden)
import Html exposing (Html)
import List.Extra as List
import List.Nonempty as Nonempty exposing (Nonempty)
import Parser exposing ((|.), (|=), Parser, Trailing(..), backtrackable, keyword, oneOf, problem, run, sequence, spaces, succeed, symbol, token, variable)
import Parser.Extras exposing (some)
import Set



---- MODEL ----


type alias Model =
    { maxId : Int
    , conceptNode : ConceptNode
    , queryResult : QueryResultIndexed
    , inputText : String
    }


example =
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


init : ( Model, Cmd Msg )
init =
    let
        ( id, node ) =
            example
    in
    ( { maxId = id
      , conceptNode = node
      , queryResult = []
      , inputText = ""
      }
    , Cmd.none
    )


type alias Id =
    Int


type alias Index =
    Int


type alias Query =
    { target : SubQuery
    , subQueries : List SubQuery
    }


type alias SubQuery =
    { name : Name
    , selection : List Id
    }


type alias QueryResult =
    List Id


type alias QueryResultIndexed =
    List ( Index, Id )


type alias Command =
    ( Query, Action )


type Action
    = Delete
    | NoAction
    | Reverse


type alias ConceptNode =
    { id : Id
    , concept : Concept
    }


type Concept
    = Hole
    | Leaf LeafName String
    | Node NodeName (List ConceptNode)


type Name
    = LeafName LeafName
    | NodeName NodeName


type LeafName
    = String
    | Integer


type NodeName
    = List
    | Branch
    | Case


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
                    run command string
                        |> Result.map (\( q, a ) -> ( a, runQuery q model.conceptNode ))
                        |> Result.withDefault ( NoAction, [] )
                        |> Debug.log ""

                applyAction action_ =
                    { model
                        | queryResult = []
                        , inputText = ""
                        , conceptNode = action_ (List.map Tuple.second queryResult) model.conceptNode
                    }
            in
            Debug.log "" <|
                ( case act of
                    NoAction ->
                        { model
                            | queryResult = queryResult
                            , inputText = string
                        }

                    Delete ->
                        applyAction delete

                    Reverse ->
                        applyAction reverse
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
            [ renderConcept model.queryResult model.conceptNode
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

        tokenEl string =
            el [ alignTop ] <| text string
    in
    row []
        [ -- indicator conceptNode.id
          case conceptNode.concept of
            Leaf Integer string ->
                cEl [ tokenEl string ]

            Leaf String string ->
                cEl [ tokenEl <| "\"" ++ string ++ "\"" ]

            Node Case (pattern :: branches) ->
                cEl
                    [ row [] [ tokenEl "case", el [ paddingXY 5 0 ] <| renderConcept queryResult pattern, tokenEl "of" ]
                    , column [ paddingXY 25 0 ] <| List.map (renderConcept queryResult) branches
                    ]

            Node List [] ->
                cEl [ tokenEl "[]" ]

            Node List (e :: es) ->
                cEl <|
                    row [] [ tokenEl "[", renderConcept queryResult e ]
                        :: List.map (\it -> row [] [ el [ alignTop ] (tokenEl ","), renderConcept queryResult it ]) es
                        ++ [ tokenEl "]" ]

            Node Branch [ pattern, expr ] ->
                cEl <|
                    [ row [] [ renderConcept queryResult pattern, tokenEl " -> " ]
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


runQuery : Query -> ConceptNode -> QueryResultIndexed
runQuery query root =
    let
        subQueries =
            query.subQueries ++ [ query.target ]

        findMatches : Name -> ConceptNode -> List ConceptNode
        findMatches name node =
            if match name node then
                node :: List.concatMap (findMatches name) (getChildren node)

            else
                List.concatMap (findMatches name) (getChildren node)

        findFilteredMatches : SubQuery -> ConceptNode -> List ConceptNode
        findFilteredMatches subQuery node =
            if List.isEmpty subQuery.selection then
                findMatches subQuery.name node

            else
                findMatches subQuery.name node
                    |> List.indexedMap Tuple.pair
                    |> List.filter (\( i, _ ) -> List.member i subQuery.selection)
                    |> List.map Tuple.second

        go : SubQuery -> List ConceptNode -> List ConceptNode
        go subQuery nodes =
            List.concatMap (findFilteredMatches subQuery) nodes
    in
    List.foldl go [ root ] subQueries
        |> List.map .id
        |> List.indexedMap Tuple.pair


match : Name -> ConceptNode -> Bool
match name node =
    case ( name, node.concept ) of
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


reverse : QueryResult -> ConceptNode -> ConceptNode
reverse queryResult conceptNode =
    { conceptNode
        | concept =
            case ( List.member conceptNode.id queryResult, conceptNode.concept ) of
                ( True, Node a list ) ->
                    list
                        |> List.reverse
                        |> List.map (reverse queryResult)
                        |> Node a

                ( False, Node name children ) ->
                    Node name <| List.map (reverse queryResult) children

                ( _, otherwise ) ->
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



---- Parsers ----


command : Parser Command
command =
    succeed Tuple.pair
        |= parseQuery
        |= oneOf
            [ backtrackable <| succeed identity |. spaces |= action |. symbol " "
            , succeed NoAction
            ]


action : Parser Action
action =
    oneOf
        [ Parser.map (always Delete) <| oneOf [ keyword "delete", keyword "d" ]
        , Parser.map (always Reverse) <| oneOf [ keyword "reverse", keyword "r" ]
        ]


parseQuery : Parser Query
parseQuery =
    let
        toQuery names =
            List.unconsLast names
                |> Maybe.map (\( name, queries ) -> succeed <| Query name queries)
                |> Maybe.withDefault (problem "empty query")
    in
    sequence { start = "", separator = ".", end = "", spaces = spaces, item = parseSubQuery, trailing = Forbidden }
        |> Parser.andThen toQuery


parseInt : Parser Int
parseInt =
    variable { start = Char.isDigit, inner = Char.isDigit, reserved = Set.empty }
        |> Parser.andThen
            (String.toInt >> Maybe.map succeed >> Maybe.withDefault (problem "invalid integer"))


parseSubQuery : Parser SubQuery
parseSubQuery =
    succeed SubQuery
        |= parseName
        |= oneOf
            [ backtrackable <| succeed List.singleton |= parseInt
            , succeed []
            ]


fromToken : String -> a -> Parser a
fromToken string a =
    Parser.map (always a) <| token string


parseName : Parser Name
parseName =
    oneOf
        [ leafName |> Parser.map LeafName
        , nodeName |> Parser.map NodeName
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
