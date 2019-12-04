module Tests exposing (suite)

import Expect
import TOML.Parser exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "TOML.Parser"
        [ describe "Booleans"
            [ test "Spec 1" <|
                \_ ->
                    fromString "bool1 = true\nbool2 = false"
                        |> Expect.equal (Ok [ ( "bool1", VBoolean True ), ( "bool2", VBoolean False ) ])
            ]
        ]
