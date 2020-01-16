module Main exposing (..)

import Bool.Extra exposing (ifElse)
import Browser
import Browser.Events exposing (onKeyDown)
import Element exposing (Color, Element, alignLeft, alignTop, column, el, fill, paddingXY, px, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font exposing (Font)
import Element.Input as Input exposing (focusedOnLoad, labelHidden, placeholder)
import Html exposing (Html)
import Json.Decode
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import List.Extra as List
import List.Nonempty as Nonempty exposing (Nonempty)
import Parser exposing ((|.), (|=), Parser, Trailing(..), andThen, backtrackable, chompUntil, end, getChompedString, keyword, lazy, map, oneOf, problem, run, sequence, spaces, succeed, symbol, token, variable)
import Parser.Extras exposing (between, many, parens, some)
import Set


{-| Displays debug information in the view
-}
debug : Bool
debug =
    False |> Debug.log "debug mode"



---- MODEL ----


type alias Model =
    { maxId : Int
    , root : ConceptNode
    , selectedNodes : List ConceptNode
    , mode : Mode
    }


type Mode
    = Normal
    | Select String


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
            [ ident "foo"
            , li
                [ li [ int 1 ]
                , ca
                    [ ident "bar"
                    , br
                        [ str "abc"
                        , li
                            [ int 2
                            , int 3
                            , ca
                                [ ident "baz"
                                , br [ str "b", int 4 ]
                                , br [ str "c", int 5 ]
                                ]
                            ]
                        ]
                    , br
                        [ str "d"
                        , li [ int 6, int 7, int 8 ]
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
      , root = node
      , selectedNodes = []
      , mode = Select ""
      }
    , Cmd.none
    )


type alias Id =
    Int


type alias Index =
    Int


type alias Query =
    List SubQuery


type SubQuery
    = NameQuery Name
    | NumberQuery (List Int)
    | ChildQuery


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
    | Add ConceptNode


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
    | HoleName


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
    | HandleKeyboardEvent KeyboardEvent


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        TextInput string ->
            let
                result =
                    run parseQuery string
                        |> Result.map (\q -> runQuery q [ model.root ])
                        |> Result.withDefault model.selectedNodes
            in
            ( { model | selectedNodes = result, mode = Select string }, Cmd.none )

        HandleKeyboardEvent keyboardEvent ->
            let
                x =
                    Debug.log "keyboard event" keyboardEvent
            in
            case ( keyboardEvent.ctrlKey, keyboardEvent.key ) of
                ( True, Just "r" ) ->
                    ( doAction Reverse model, Cmd.none )

                ( True, Just "d" ) ->
                    ( doAction Delete model, Cmd.none )

                _ ->
                    ( model, Cmd.none )


doAction : Action -> Model -> Model
doAction action model =
    let
        go f clear =
            { model
                | root = change f (List.map .id model.selectedNodes) model.root
                , mode =
                    if clear then
                        Select ""

                    else
                        model.mode
            }
    in
    case action of
        Delete ->
            go delete True

        NoAction ->
            model

        Reverse ->
            go reverse False

        Add conceptNode ->
            Debug.todo ""



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
            [ renderConcept (model.selectedNodes |> List.map .id |> List.indexedMap Tuple.pair) model.root
            , case model.mode of
                Select string ->
                    Input.text [ focusedOnLoad ]
                        { onChange = TextInput
                        , text = string
                        , placeholder = Nothing
                        , label = labelHidden "command input field"
                        }

                _ ->
                    Element.none
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
                    [ el [ Background.color (rgb255 255 110 110) ] (text <| String.fromInt conceptNode.id)
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


runQuery : Query -> List ConceptNode -> List ConceptNode
runQuery query ns =
    let
        findMatches : Name -> ConceptNode -> List ConceptNode
        findMatches name node =
            if match name node then
                node :: List.concatMap (findMatches name) (getChildren node)

            else
                List.concatMap (findMatches name) (getChildren node)

        executeSubQuery : SubQuery -> List ConceptNode -> List ConceptNode
        executeSubQuery subQuery nodes =
            case subQuery of
                NameQuery name ->
                    List.concatMap (findMatches name) nodes

                NumberQuery selection ->
                    nodes
                        |> List.indexedMap Tuple.pair
                        |> List.filter (\( i, _ ) -> List.member i selection)
                        |> List.map Tuple.second

                ChildQuery ->
                    List.concatMap getChildren nodes
    in
    List.foldl executeSubQuery ns query


match : Name -> ConceptNode -> Bool
match name node =
    case ( name, node.concept ) of
        ( LeafName n1, Leaf n2 _ ) ->
            n1 == n2

        ( NodeName n1, Node n2 _ ) ->
            n1 == n2

        ( HoleName, Hole ) ->
            True

        _ ->
            False


newNode : NodeName -> List ConceptNode -> Maybe Concept
newNode name nodes =
    Maybe.map (Node name) <|
        case ( name, nodes ) of
            ( List, _ ) ->
                Just <| List.filter (not << match HoleName) nodes

            ( Branch, [ pattern, expression ] ) ->
                Just <| nodes

            ( Case, expression :: branch :: branches ) ->
                Just <|
                    expression
                        :: (Nonempty.Nonempty branch branches
                                |> Nonempty.filter (not << match HoleName) { branch | concept = Hole }
                                |> Nonempty.toList
                           )

            ( Assignment, [ _, _ ] ) ->
                Just <| nodes

            _ ->
                Nothing


change : (Concept -> Concept) -> QueryResult -> ConceptNode -> ConceptNode
change f results node =
    { node
        | concept =
            ifElse f identity (List.member node.id results) <|
                case node.concept of
                    Node name children ->
                        List.map (change f results) children
                            |> newNode name
                            |> Maybe.withDefault node.concept

                    a ->
                        a
    }


delete : Concept -> Concept
delete _ =
    Hole


reverse : Concept -> Concept
reverse concept =
    case concept of
        Node Case (e :: bs) ->
            Node Case (e :: List.reverse bs)

        Node name children ->
            Node name (List.reverse children)

        Leaf String string ->
            Leaf String (String.reverse string)

        a ->
            a


add : Concept -> Concept -> Concept
add arg concept =
    case concept of
        Node name list ->
            Node name <| list ++ [ ConceptNode -1 arg ]

        Hole ->
            arg

        a ->
            a


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
        , subscriptions = \x -> onKeyDown (Json.Decode.map HandleKeyboardEvent decodeKeyboardEvent)
        }



---- Parsers ----


command : Parser Command
command =
    succeed Tuple.pair
        |= parseQuery
        |= oneOf
            [ backtrackable <| succeed identity |. spaces |= parseAction |. symbol " "
            , succeed NoAction
            ]


parseAction : Parser Action
parseAction =
    oneOf
        [ succeed Delete |. keyword "d"
        , succeed Reverse |. keyword "r"
        , succeed Add |. token "a" |= parseConcept
        ]


parseConcept : Parser ConceptNode
parseConcept =
    oneOf
        [ succeed (\s -> ConceptNode 0 <| Leaf Integer (String.fromInt s)) |= parseInt
        , succeed (ConceptNode 0 << Leaf String) |. symbol "\"" |= getChompedString (chompUntil "\"") |. symbol "\""
        , succeed (\name children -> ConceptNode 0 <| Node name children)
            |= nodeName
            |= parens (many <| lazy (\x -> parseConcept))
        ]


parseQuery : Parser Query
parseQuery =
    some parseSubQuery |> map (\( x, y ) -> x :: y)


parseInt : Parser Int
parseInt =
    variable { start = Char.isDigit, inner = Char.isDigit, reserved = Set.empty }
        |> Parser.andThen
            (String.toInt >> Maybe.map succeed >> Maybe.withDefault (problem "invalid integer"))


parseSubQuery : Parser SubQuery
parseSubQuery =
    oneOf
        [ map NameQuery parseName
        , map NumberQuery parseSelection
        , succeed ChildQuery |. symbol "."
        ]


parseSelection : Parser (List Int)
parseSelection =
    parseInt
        |> andThen
            (\y ->
                oneOf
                    [ succeed (List.range y) |. symbol "-" |= parseInt
                    , many (symbol "," |> andThen (\_ -> parseInt)) |> map ((::) y)
                    ]
            )


fromToken : String -> a -> Parser a
fromToken string a =
    Parser.map (always a) <| token string


parseName : Parser Name
parseName =
    oneOf
        [ leafName |> Parser.map LeafName
        , nodeName |> Parser.map NodeName
        , symbol "h" |> map (always HoleName)
        ]


leafName : Parser LeafName
leafName =
    oneOf
        [ fromToken "s" String
        , fromToken "id" Identifier
        , fromToken "i" Integer
        , fromToken "ch" Character
        ]


nodeName : Parser NodeName
nodeName =
    oneOf
        [ fromToken "l" List
        , fromToken "br" Branch
        , fromToken "ca" Case
        , fromToken "as" Assignment
        ]
