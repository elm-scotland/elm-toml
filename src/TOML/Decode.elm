module TOML.Decode exposing (Decoder(..))


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
