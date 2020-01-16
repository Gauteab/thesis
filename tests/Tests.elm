module Tests exposing (..)

import Expect
import Main exposing (..)
import Parser exposing ((|.), end, symbol)
import Parser.Extras exposing (many)
import Test exposing (..)



-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


parsers : Test
parsers =
    describe "parsers" <|
        let
            testParser description parser input expected =
                test description <|
                    \_ ->
                        Expect.equal expected <| Parser.run (parser |. end) input

            li =
                NodeName List

            int =
                LeafName Integer
        in
        [ testParser "name sub-query" parseSubQuery "i" <| Ok (NameQuery <| LeafName Integer)
        , testParser "name query" parseQuery "i" <| Ok [ NameQuery <| LeafName Integer ]
        , testParser "empty query" (many parseSubQuery) "" <| Ok []
        , testParser "concept" parseConcept "\"hello\"" <| Ok (ConceptNode 0 <| Leaf String "hello")
        , testParser "nested concept" parseConcept "l(1)" <| Ok (ConceptNode 0 <| Node List [ ConceptNode 0 <| Leaf Integer "1" ])
        ]



--queries : Test
--queries =
--    describe "queries" <|
--        [ test "query" <| \_ -> Expect.equal runQuery ]
