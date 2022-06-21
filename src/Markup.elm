module Markup exposing
    ( Markup, node, tagNode, text
    , Tag, tag
    , Key, key
    , Attribute, at, ev, attribute, event
    )

{-|


# Markup

@docs Markup, node, tagNode, text
@docs Tag, tag
@docs Key, key


# Attribute

@docs Attribute, at, ev, attribute, event

-}

import Bitwise as Bit
import FNV1a
import Json.Encode as Encode exposing (Value)
import Json.HashEncode as HashEncode exposing (HashedValue)



-- Markup


type Markup
    = Markup ( Seed, Value )


type alias Seed =
    Int


type Tag
    = Tag Seed Value


type Key
    = Key Seed String Value


tag : String -> Tag
tag str =
    Tag (FNV1a.hashWithSeed str seedForTag) (Encode.string str)


key : String -> Key
key str =
    Key (FNV1a.hashWithSeed str seedForKey) str (Encode.string str)


tagNode : Tag -> List Attribute -> List ( Key, Markup ) -> Markup
tagNode (Tag tagSeed tagValue) attrs entries =
    let
        attrsSeed =
            hashAttributes attrs

        entriesSeed =
            hashEntries entries

        seed =
            joinSeed entriesSeed (joinSeed attrsSeed tagSeed)
    in
    ( seed
    , Encode.object
        [ ( "tag", tagValue )
        , ( "hash", Encode.int seed )
        , ( "attrsHash", Encode.int attrsSeed )
        , ( "attrs", encodeAttributes attrs )
        , ( "entriesHash", Encode.int entriesSeed )
        , ( "entries", encodeEntries entries )
        , ( "keyed", encodeKeyed entries )
        ]
    )
        |> Markup


node : List Attribute -> List ( Key, Markup ) -> Markup
node attrs entries =
    let
        attrsSeed =
            hashAttributes attrs

        entriesSeed =
            hashEntries entries

        seed =
            joinSeed attrsSeed entriesSeed
    in
    ( seed
    , Encode.object
        [ ( "hash", Encode.int seed )
        , ( "attrsHash", Encode.int attrsSeed )
        , ( "attrs", encodeAttributes attrs )
        , ( "entriesHash", Encode.int entriesSeed )
        , ( "entries", encodeEntries entries )
        , ( "keyed", encodeKeyed entries )
        ]
    )
        |> Markup


text : String -> Markup
text str =
    Markup ( FNV1a.hashWithSeed str seedForText, Encode.string str )



-- Markup internals


hashAttributes : List Attribute -> Seed
hashAttributes =
    List.foldl
        (\(Attribute keySeed valueSeed _) -> combineSeed (joinSeed keySeed valueSeed))
        (FNV1a.hash "attributes")


encodeAttributes : List Attribute -> Value
encodeAttributes =
    List.map
        (\(Attribute _ _ pair) -> pair)
        >> Encode.object


seedForTag : Seed
seedForTag =
    FNV1a.hash "tag"


seedForKey : Seed
seedForKey =
    FNV1a.hash "key"


hashEntries : List ( Key, Markup ) -> Seed
hashEntries =
    List.foldl
        (\( Key keySeed _ _, Markup ( valueSeed, _ ) ) -> combineSeed (joinSeed keySeed valueSeed))
        (FNV1a.hash "entries")


encodeKeyed : List ( Key, Markup ) -> Value
encodeKeyed =
    List.map
        (\( Key _ key_ _, Markup ( _, value ) ) -> ( key_, value ))
        >> Encode.object


encodeEntries : List ( Key, Markup ) -> Value
encodeEntries =
    Encode.list <|
        \( Key _ _ key_, Markup ( _, value ) ) ->
            Encode.object
                [ ( "key", key_ )
                , ( "value", value )
                ]


seedForText : Seed
seedForText =
    FNV1a.hash "text"



-- Attributes


type Attribute
    = Attribute Seed Seed ( String, Value )


at : String -> HashedValue -> Attribute
at str value =
    let
        valueSeed =
            HashEncode.toHash value
    in
    Attribute
        (FNV1a.hashWithSeed str seedForAttribute)
        valueSeed
        ( str, encodeAttribute valueSeed (HashEncode.toValue value) )


ev : String -> HashedValue -> Attribute
ev str value =
    let
        valueSeed =
            joinSeed (HashEncode.toHash value) seedForEvent
    in
    Attribute
        (FNV1a.hashWithSeed str seedForAttribute)
        valueSeed
        ( str, encodeEvent valueSeed (HashEncode.toValue value) )


attribute : String -> HashedValue -> Attribute
attribute str =
    let
        keySeed =
            FNV1a.hashWithSeed str seedForAttribute
    in
    \value ->
        let
            valueSeed =
                HashEncode.toHash value
        in
        Attribute
            keySeed
            valueSeed
            ( str, encodeAttribute valueSeed (HashEncode.toValue value) )


event : String -> HashedValue -> Attribute
event str =
    let
        keySeed =
            FNV1a.hashWithSeed str seedForAttribute
    in
    \value ->
        let
            valueSeed =
                joinSeed (HashEncode.toHash value) seedForEvent
        in
        Attribute
            keySeed
            valueSeed
            ( str, encodeEvent valueSeed (HashEncode.toValue value) )



-- Attributes internals


seedForAttribute : Seed
seedForAttribute =
    FNV1a.hash "attribute"


encodeAttribute : Seed -> Value -> Value
encodeAttribute seed context =
    Encode.object
        [ ( "hash", Encode.int seed )
        , attributeTag
        , ( "context", context )
        ]


attributeTag : ( String, Value )
attributeTag =
    ( "event", Encode.bool False )


seedForEvent : Seed
seedForEvent =
    FNV1a.hash "event"


encodeEvent : Seed -> Value -> Value
encodeEvent seed context =
    Encode.object
        [ ( "hash", Encode.int seed )
        , eventTag
        , ( "context", context )
        ]


eventTag : ( String, Value )
eventTag =
    ( "event", Encode.bool True )



--- general internals


joinSeed : Seed -> Seed -> Seed
joinSeed a seed =
    Bit.xor (a * 16777619) seed


combineSeed : Seed -> Seed -> Seed
combineSeed a seed =
    Bit.xor a seed * 16777619
