module TOML.Decode exposing (..)

import Parser exposing (..)
import Set exposing (Set)


type Decoder
    = Decoder



--


type alias TomlTable =
    List ( String, TomlValue )


type TomlBasic
    = BString String
    | BInt Int
    | BFloat Float
    | BBoolean Bool
    | BDateTime String
    | BLocalDateTime String
    | BLocalDate String
    | BLocalTime String


type TomlList
    = LString (List String)
    | LInt (List Int)
    | LFloat (List Float)
    | LDateTime (List String)
    | LLocalDateTime (List String)
    | LLocalDate (List String)
    | LLocalTime (List String)
    | LList (List TomlList)
    | LTable (List TomlTable)


type TomlValue
    = VBasic TomlBasic
    | VList TomlList
    | VTable String TomlTable



--


parse : Parser TomlTable
parse =
    Debug.todo "working on it..."


parseKey : Parser String
parseKey =
    oneOf
        [ parseBareKey
        , parseSingleQuotedKey
        , parseDoubleQuotedKey
        ]


parseBareKey : Parser String
parseBareKey =
    let
        allowed ch =
            Char.isAlphaNum ch || ch == '_' || ch == '-'
    in
    Parser.variable
        { start = allowed
        , inner = allowed
        , reserved = Set.empty
        }


parseSingleQuotedKey : Parser String
parseSingleQuotedKey =
    succeed identity
        |. symbol "'"
        |= (chompUntil "'" |> getChompedString)
        |. symbol "'"


parseDoubleQuotedKey : Parser String
parseDoubleQuotedKey =
    succeed identity
        |. symbol "\""
        |= (chompUntil "\"" |> getChompedString)
        |. symbol "\""


parseLine : Parser ( String, TomlValue )
parseLine =
    Parser.succeed Tuple.pair
        |. spaces
        |= parseKey
        |. spaces
        |. symbol "="
        |. spaces
        |= succeed (VBasic (BString "todo"))
