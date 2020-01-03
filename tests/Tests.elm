module Tests exposing (..)

import Expect
import Main exposing (Action(..), action)
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
        ]
