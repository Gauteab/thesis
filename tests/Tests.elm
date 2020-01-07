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
        [ testParser "name sub-query" parseSubQuery "int" <| Ok (NameQuery <| LeafName Integer)
        , testParser "name query" parseQuery "int" <| Ok [ NameQuery <| LeafName Integer ]
        , testParser "empty query" (many parseSubQuery) "" <| Ok []
        ]



--queries : Test
--queries =
--    describe "queries" <|
--        [ test "query" <| \_ -> Expect.equal runQuery ]
