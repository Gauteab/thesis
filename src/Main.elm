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


{-| Displays debug information in the view
-}
debug : Bool
debug =
    False |> Debug.log "debug mode"



---- MODEL ----


type alias Model =
    { maxId : Int
    , conceptNode : ConceptNode
    , queryResult : QueryResultIndexed
    , inputText : String
    }


example =
    let
        node =
            ConceptNode 0

        int =
            node << Leaf Integer << String.fromInt

        str =
            node << Leaf String

        li =
            node << Node List

        ca =
            node << Node Case

        br =
            node << Node Branch

        assignment =
            node << Node Assignment

        ident =
            node << Leaf Identifier

        ch =
            node << Leaf Character << String.fromChar
    in
    assignIds 0 <|
        assignment
            [ ident "hello"
            , li
                [ int 1
                , li [ int 6 ]
                , ca
                    [ ident "name"
                    , br
                        [ str "a"
                        , li
                            [ int 2
                            , int 3
                            , ca
                                [ str "xyz"
                                , br [ str "b", li [ int 4, str "c", ch 'c' ] ]
                                ]
                            ]
                        ]
                    , br
                        [ int 5
                        , int 6
                        ]
                    ]
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
    | Identifier
    | Character


type NodeName
    = List
    | Branch
    | Case
    | Assignment



--type Expression
--    = Character Char
--    | String String
--    | Integer Int
--    | Float Float
--    | Variable (List String)
--    | List (List Expression)
--    | Tuple (List Expression)
--    | Access Expression (List String)
--    | AccessFunction String
--    | Record (List ( String, Expression ))
--    | RecordUpdate String (List ( String, Expression ))
--    | If Expression Expression Expression
--    | Let (List ( Expression, Expression )) Expression
--    | Case Expression (List ( Expression, Expression ))
--    | Lambda (List Expression) Expression
--    | Application Expression Expression
--    | BinOp Expression Expression Expression
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
    Element.layout
        [ Font.family [ Font.monospace ]
        , Font.color (rgb255 0 0 0)
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

        keywordEl string =
            el [ Font.color (rgb255 137 89 168), alignTop ] <| text string

        tokenEl string =
            el [ alignTop ] <| text string
    in
    row []
        [ if debug then
            indicator conceptNode.id

          else
            Element.none
        , cEl <|
            case conceptNode.concept of
                Leaf Integer string ->
                    [ el [ Font.color (rgb255 26 79 171) ] <| text string ]

                Leaf String string ->
                    [ el [ Font.color (rgb255 33 122 17) ] <| text <| "\"" ++ string ++ "\"" ]

                Leaf Character string ->
                    [ el [ Font.color (rgb255 33 122 17) ] <| text <| "'" ++ string ++ "'" ]

                Leaf Identifier string ->
                    [ text string ]

                Node Case (pattern :: branches) ->
                    [ row [] [ keywordEl "case", el [ paddingXY 5 0 ] <| renderConcept queryResult pattern, keywordEl "of" ]
                    , column [ paddingXY 25 0 ] <| List.map (renderConcept queryResult) branches
                    ]

                Node List [] ->
                    [ tokenEl "[]" ]

                Node List (e :: es) ->
                    row [] [ tokenEl "[", renderConcept queryResult e ]
                        :: List.map (\it -> row [] [ el [ alignTop ] (tokenEl ","), renderConcept queryResult it ]) es
                        ++ [ tokenEl "]" ]

                Node Branch [ pattern, expr ] ->
                    [ row [] [ renderConcept queryResult pattern, tokenEl " -> " ]
                    , el [ paddingXY 25 0 ] (renderConcept queryResult expr)
                    ]

                Node Assignment [ name, expression ] ->
                    [ row [] [ renderConcept queryResult name, tokenEl " = " ]
                    , el [ paddingXY 25 0 ] (renderConcept queryResult expression)
                    ]

                Node Assignment _ ->
                    Debug.todo "invalid assignment"

                Hole ->
                    [ el
                        [ Background.color (rgb255 255 110 110) ]
                      <|
                        text (String.fromInt conceptNode.id)
                    ]

                Node Branch _ ->
                    Debug.todo "invalid branch"

                Node Case _ ->
                    Debug.todo "invalid case expression"
        ]


isMultiline : ConceptNode -> Bool
isMultiline conceptNode2 =
    case conceptNode2.concept of
        Node Assignment _ ->
            True

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


assignIds : Int -> ConceptNode -> ( Int, ConceptNode )
assignIds count conceptNode =
    case conceptNode.concept of
        Node name children ->
            let
                rec node ( currentCount, currentConcepts ) =
                    assignIds currentCount node
                        |> Tuple.mapSecond (\newConcept -> newConcept :: currentConcepts)

                ( newCount, newChildren ) =
                    List.foldr rec ( count + 1, [] ) children
            in
            ( newCount, ConceptNode count <| Node name newChildren )

        _ ->
            ( count + 1, { conceptNode | id = count } )


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
        , fromToken "id" Identifier
        , fromToken "char" Character
        ]


nodeName : Parser NodeName
nodeName =
    oneOf
        [ fromToken "list" List
        , fromToken "branch" Branch
        , fromToken "case" Case
        , fromToken "ass" Assignment
        ]
