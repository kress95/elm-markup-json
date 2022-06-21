module Markup exposing (Attribute, Key, Markup, Tag, key, node, tag, tagNode, text)

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


tag : String -> Tag
tag str =
    Tag (FNV1a.hashWithSeed str tagSeed) (Encode.string str)


key : String -> Key
key str =
    Key (FNV1a.hashWithSeed str keySeed) str (Encode.string str)


tagNode : Tag -> List Attribute -> List ( Key, Markup ) -> Markup
tagNode (Tag tagHash tagValue) attrs entries =
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


tagSeed : Hash
tagSeed =
    FNV1a.hash "tag"


keySeed : Hash
keySeed =
    FNV1a.hash "key"


hashEntries : List ( Key, Markup ) -> Hash
hashEntries =
    List.foldl
        (\( Key kHash _ _, Markup ( vHash, _ ) ) -> Hash.combine (Hash.join kHash vHash))
        (FNV1a.hash "entries")


encodeCache : List ( Key, Markup ) -> Value
encodeCache =
    List.map
        (\( Key _ k _, Markup ( _, v ) ) -> ( k, v ))
        >> Encode.object


encodeEntries : List ( Key, Markup ) -> Value
encodeEntries =
    Encode.list <|
        \( Key _ _ k, Markup ( _, v ) ) ->
            Encode.object
                [ ( "key", k )
                , ( "value", v )
                ]


textSeed : Hash
textSeed =
    FNV1a.hash "text"
