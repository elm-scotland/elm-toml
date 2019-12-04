module TOML.Parser exposing (TomlList(..), TomlTable, TomlValue(..), fromString)

import Parser exposing ((|.), (|=), DeadEnd, Parser, Step(..))
import Set


type alias TomlTable =
    List ( String, TomlValue )


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
    = VString String
    | VInt Int
    | VFloat Float
    | VBoolean Bool
    | VDateTime String
    | VLocalDateTime String
    | VLocalDate String
    | VLocalTime String
    | VList TomlList
    | VTable String TomlTable


type alias State =
    TomlTable


fromString : String -> Result (List DeadEnd) TomlTable
fromString =
    Parser.run mainLoop


emptyState : State
emptyState =
    []


mainLoop : Parser TomlTable
mainLoop =
    Parser.loop emptyState mainLoopHelp


mainLoopHelp : State -> Parser (Step State TomlTable)
mainLoopHelp state =
    Parser.oneOf
        [ Parser.map (always (Done (List.reverse state))) Parser.end
        , Parser.map (\kv -> Loop (kv :: state)) keyValue
        , Parser.map (always (Loop state)) (Parser.oneOf [ comment, lineBreak, whitespace ])
        ]


key : Parser String
key =
    Parser.oneOf
        [ bareKey
        , literalString
        , basicString
        ]


bareKey : Parser String
bareKey =
    let
        allowed ch =
            Char.isAlphaNum ch || ch == '_' || ch == '-'
    in
    Parser.variable
        { start = allowed
        , inner = allowed
        , reserved = Set.empty
        }


literalString : Parser String
literalString =
    Parser.succeed identity
        |. Parser.symbol "'"
        |= (Parser.chompUntil "'" |> Parser.getChompedString)
        |. Parser.symbol "'"


basicString : Parser String
basicString =
    Parser.succeed identity
        |. Parser.symbol "\""
        |= (Parser.chompUntil "\"" |> Parser.getChompedString)
        |. Parser.symbol "\""


bool : Parser TomlValue
bool =
    Parser.map VBoolean <|
        Parser.oneOf
            [ Parser.succeed True
                |. Parser.keyword "true"
            , Parser.succeed False
                |. Parser.keyword "false"
            ]


keyValue : Parser ( String, TomlValue )
keyValue =
    Parser.succeed Tuple.pair
        |= key
        |. whitespace
        |. Parser.symbol "="
        |. whitespace
        |= Parser.oneOf [ bool ]


comment : Parser ()
comment =
    Parser.lineComment "#"


whitespace : Parser ()
whitespace =
    Parser.chompWhile (\c -> c == ' ' || c == '\t')


lineBreak : Parser ()
lineBreak =
    Parser.token "\n"
