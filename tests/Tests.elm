module Tests exposing (..)

import Expect
import Main exposing (..)
import Parser exposing ((|.), end)
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

            sq =
                SubQuery

            q a b =
                Ok <| Query a b
        in
        [ testParser "action" action "delete" (Ok Delete)
        , testParser "target" parseSubQuery "list" <| Ok (sq li [])
        , testParser "target with id" parseSubQuery "list1" <| Ok (SubQuery (NodeName List) [ 1 ])
        , testParser "query" parseQuery "list1" <| q (sq li [ 1 ]) []
        , testParser "query with leaf" parseQuery "list.int" <| q (sq int []) [ sq li [] ]
        , testParser "query with leaf and id" parseQuery "list1.int1" <| q (sq int [ 1 ]) [ sq li [ 1 ] ]
        ]



--queries : Test
--queries =
--    describe "queries" <|
--        [ test "query" <| \_ -> Expect.equal runQuery ]
