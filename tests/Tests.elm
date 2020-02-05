module Tests exposing (suite)

import Expect
import TOML.Parser exposing (TomlList(..), TomlValue(..), fromString)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "TOML.Parser"
        [ describe "Comments"
            [ test "Spec comments example 1" <|
                \_ ->
                    fromString "# This is a full-line comment\nkey = \"value\" # This is a comment at the end of a line"
                        |> Expect.equal (Ok [ ( "key", VString "value" ) ])
            ]
        , describe "Key/Value Pair"
            [ test "Spec key/value pair example 1" <|
                \_ ->
                    fromString "key = \"value\""
                        |> Expect.equal (Ok [ ( "key", VString "value" ) ])
            , test "Spec key/value pair example 2" <|
                \_ ->
                    fromString "key = # INVALID"
                        |> Expect.err
            ]
        , describe "String"
            [ test "Spec String example 1" <|
                \_ ->
                    fromString "str = \"I'm a string. \\\"You can quote me\\\". Name\\tJos\\u00E9\\nLocation\\tSF.\""
                        |> Expect.equal (Ok [ ( "str", VString "I'm a string. \"You can quote me\". Name\tJosé\nLocation\tSF." ) ])
            ]
        , describe "Booleans"
            [ test "Spec Boolean example 1" <|
                \_ ->
                    fromString "bool1 = true\nbool2 = false"
                        |> Expect.equal (Ok [ ( "bool1", VBoolean True ), ( "bool2", VBoolean False ) ])
            ]
        ]
