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
        in
        [ testParser "action" action "delete" (Ok Delete)
        , testParser "node then leaf" parseQuery "list.int" (Ok ( LeafName Integer, [ NodeName List ] ))
        , testParser "two nodes" parseQuery "list.case" (Ok ( NodeName Case, [ NodeName List ] ))
        , testParser "command" command "list.case delete " (Ok ( ( NodeName Case, [ NodeName List ] ), Delete ))
        , testParser "command no action" command "list.case" (Ok ( ( NodeName Case, [ NodeName List ] ), NoAction ))
        ]
