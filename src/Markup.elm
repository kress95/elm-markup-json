module Markup exposing (..)

import FNV1a
import Json.Encode as Encode exposing (Value)
import Markup.Hash as Hash exposing (Hash)



-- Attributes


type Attribute
    = Attribute Hash Hash ( String, Value )


-- Attributes internals


hashAttributes : List Attribute -> Hash
hashAttributes =
    List.foldl
        (\(Attribute keyHash valueHash _) -> Hash.combine (Hash.join keyHash valueHash))
        (FNV1a.hash "attributes")


encodeAttributes : List Attribute -> Value
encodeAttributes =
    List.map
        (\(Attribute _ _ pair) -> pair)
        >> Encode.object



-- Markup


type Markup
    = Markup ( Hash, Value )


type Tag
    = Tag Hash Value


type Key
    = Key Hash String Value


tag : Tag -> List Attribute -> List ( Key, Markup ) -> Markup
tag (Tag tagHash tagValue) attrs entries =
    let
        attrsHash =
            hashAttributes attrs

        entriesHash =
            hashEntries entries

        seed =
            Hash.join entriesHash (Hash.join attrsHash tagHash)
    in
    ( seed
    , Encode.object
        [ ( "tag", tagValue )
        , ( "hash", Encode.int seed )
        , ( "attrsHash", Encode.int attrsHash )
        , ( "attrs", encodeAttributes attrs )
        , ( "entriesHash", Encode.int entriesHash )
        , ( "entries", encodeEntries entries )
        , ( "cache", encodeCache entries )
        ]
    )
        |> Markup


node : List Attribute -> List ( Key, Markup ) -> Markup
node attrs entries =
    let
        attrsHash =
            hashAttributes attrs

        entriesHash =
            hashEntries entries

        seed =
            Hash.join attrsHash entriesHash
    in
    ( seed
    , Encode.object
        [ ( "hash", Encode.int seed )
        , ( "attrsHash", Encode.int attrsHash )
        , ( "attrs", encodeAttributes attrs )
        , ( "entriesHash", Encode.int entriesHash )
        , ( "entries", encodeEntries entries )
        , ( "cache", encodeCache entries )
        ]
    )
        |> Markup


text : String -> Markup
text str =
    Markup ( FNV1a.hashWithSeed str textSeed, Encode.string str )



-- Markup internals


hashEntries : List ( Key, Markup ) -> Int
hashEntries =
    List.foldl
        (\( Key keyHash _ _, Markup ( value, _ ) ) -> Hash.combine (Hash.join keyHash value))
        (FNV1a.hash "entries")


encodeCache : List ( Key, Markup ) -> Value
encodeCache =
    List.map
        (\( Key _ key _, Markup ( _, value ) ) -> ( key, value ))
        >> Encode.object


encodeEntries : List ( Key, Markup ) -> Value
encodeEntries =
    Encode.list <|
        \( Key _ _ keyValue, Markup ( _, value ) ) ->
            Encode.object
                [ ( "key", keyValue )
                , ( "value", value )
                ]


textSeed : Int
textSeed =
    FNV1a.hash "text"
