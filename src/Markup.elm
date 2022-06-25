module Markup exposing
    ( Markup, node, htmlNode, text, isEqual, encode
    , Seed, hash
    , Tag, tag
    , Key, key
    , Attribute, attribute, event, preventDefaultEvent, stopPropagationEvent
    , defineAttribute, defineEvent, definePreventDefaultEvent, defineStopPropagationEvent
    )

{-|


# Markup

@docs Markup, node, htmlNode, text, lazy, isEqual, encode
@docs Seed, hash
@docs Tag, tag
@docs Key, key


# Memo

@docs Memo, memoInit, memo, memoWith


# Attribute

@docs Attribute, attribute, event, preventDefaultEvent, stopPropagationEvent
@docs defineAttribute, defineEvent, definePreventDefaultEvent, defineStopPropagationEvent

-}

import Bitwise as Bit
import FNV1a
import Json.Encode as Encode exposing (Value)
import Markup.Json.Encode as HashEncode exposing (MarkupValue)



-- Markup


type Markup
    = Markup Seed Value


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


htmlNode : Tag -> List Attribute -> List ( Key, Markup ) -> Markup
htmlNode (Tag tagSeed tagValue) attrs entries =
    let
        attrsSeed =
            hashAttributes attrs

        entriesSeed =
            hashEntries entries

        seed =
            joinSeed entriesSeed (joinSeed attrsSeed tagSeed)
    in
    Markup
        seed
        (Encode.object
            [ ( "hash", Encode.int seed )
            , ( "tag", tagValue )
            , ( "attrsHash", Encode.int attrsSeed )
            , ( "attrs", encodeAttributes attrs )
            , ( "entriesHash", Encode.int entriesSeed )
            , ( "entries", encodeEntries entries )
            ]
        )


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
    Markup
        seed
        (Encode.object
            [ ( "hash", Encode.int seed )
            , ( "attrsHash", Encode.int attrsSeed )
            , ( "attrs", encodeAttributes attrs )
            , ( "entriesHash", Encode.int entriesSeed )
            , ( "entries", encodeEntries entries )
            ]
        )


text : String -> Markup
text str =
    Markup (FNV1a.hashWithSeed str seedForText) (Encode.string str)


isEqual : Markup -> Markup -> Bool
isEqual (Markup a _) (Markup b _) =
    a == b


hash : Markup -> Seed
hash (Markup seed _) =
    seed


encode : Markup -> Value
encode (Markup _ value) =
    value



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
        (\( Key keySeed _ _, Markup valueSeed _ ) -> combineSeed (joinSeed keySeed valueSeed))
        (FNV1a.hash "entries")


encodeEntries : List ( Key, Markup ) -> Value
encodeEntries =
    Encode.list <|
        \( Key _ _ key_, Markup _ value ) ->
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


attribute : String -> MarkupValue -> Attribute
attribute str value =
    let
        valueSeed =
            HashEncode.hash value
    in
    Attribute
        (FNV1a.hashWithSeed str seedForAttribute)
        valueSeed
        ( str, encodeAttribute valueSeed (HashEncode.value value) )


defineAttribute : String -> MarkupValue -> Attribute
defineAttribute str =
    let
        keySeed =
            FNV1a.hashWithSeed str seedForAttribute
    in
    \value ->
        let
            valueSeed =
                HashEncode.hash value
        in
        Attribute
            keySeed
            valueSeed
            ( str, encodeAttribute valueSeed (HashEncode.value value) )


event : String -> MarkupValue -> Attribute
event str value =
    let
        valueSeed =
            joinSeed (HashEncode.hash value) seedForEvent
    in
    Attribute
        (FNV1a.hashWithSeed str seedForAttribute)
        valueSeed
        ( str, encodeEvent valueSeed (HashEncode.value value) )


preventDefaultEvent : String -> MarkupValue -> Attribute
preventDefaultEvent str value =
    let
        valueSeed =
            joinSeed (HashEncode.hash value) seedForPreventDefaultEvent
    in
    Attribute
        (FNV1a.hashWithSeed str seedForAttribute)
        valueSeed
        ( str, encodeStopPropagationEvent valueSeed (HashEncode.value value) )


stopPropagationEvent : String -> MarkupValue -> Attribute
stopPropagationEvent str value =
    let
        valueSeed =
            joinSeed (HashEncode.hash value) seedForStopPropagationEvent
    in
    Attribute
        (FNV1a.hashWithSeed str seedForAttribute)
        valueSeed
        ( str, encodeStopPropagationEvent valueSeed (HashEncode.value value) )


defineEvent : String -> MarkupValue -> Attribute
defineEvent str =
    let
        keySeed =
            FNV1a.hashWithSeed str seedForAttribute
    in
    \value ->
        let
            valueSeed =
                joinSeed (HashEncode.hash value) seedForEvent
        in
        Attribute
            keySeed
            valueSeed
            ( str, encodeEvent valueSeed (HashEncode.value value) )


definePreventDefaultEvent : String -> MarkupValue -> Attribute
definePreventDefaultEvent str =
    let
        keySeed =
            FNV1a.hashWithSeed str seedForAttribute
    in
    \value ->
        let
            valueSeed =
                joinSeed (HashEncode.hash value) seedForPreventDefaultEvent
        in
        Attribute
            keySeed
            valueSeed
            ( str, encodePreventDefaultEvent valueSeed (HashEncode.value value) )


defineStopPropagationEvent : String -> MarkupValue -> Attribute
defineStopPropagationEvent str =
    let
        keySeed =
            FNV1a.hashWithSeed str seedForAttribute
    in
    \value ->
        let
            valueSeed =
                joinSeed (HashEncode.hash value) seedForStopPropagationEvent
        in
        Attribute
            keySeed
            valueSeed
            ( str, encodeStopPropagationEvent valueSeed (HashEncode.value value) )



-- Attributes internals


seedForAttribute : Seed
seedForAttribute =
    FNV1a.hash "attribute"


encodeAttribute : Seed -> Value -> Value
encodeAttribute seed value =
    Encode.object
        [ ( "hash", Encode.int seed )
        , ( "value", value )
        ]


seedForEvent : Seed
seedForEvent =
    FNV1a.hash "event"


encodeEvent : Seed -> Value -> Value
encodeEvent seed value =
    Encode.object
        [ ( "hash", Encode.int seed )
        , eventTag
        , ( "value", value )
        ]


encodePreventDefaultEvent : Seed -> Value -> Value
encodePreventDefaultEvent seed value =
    Encode.object
        [ ( "hash", Encode.int seed )
        , eventTag
        , preventDefaultTag
        , ( "value", value )
        ]


encodeStopPropagationEvent : Seed -> Value -> Value
encodeStopPropagationEvent seed value =
    Encode.object
        [ ( "hash", Encode.int seed )
        , eventTag
        , stopPropagationTag
        , ( "value", value )
        ]


eventTag : ( String, Value )
eventTag =
    ( "event", Encode.bool True )


preventDefaultTag : ( String, Value )
preventDefaultTag =
    ( "preventDefault", Encode.bool True )


stopPropagationTag : ( String, Value )
stopPropagationTag =
    ( "stopPropagation", Encode.bool True )


seedForPreventDefaultEvent : Seed
seedForPreventDefaultEvent =
    FNV1a.hash "preventDefault"


seedForStopPropagationEvent : Seed
seedForStopPropagationEvent =
    FNV1a.hash "stopPropagation"



--- general internals


joinSeed : Seed -> Seed -> Seed
joinSeed a seed =
    Bit.xor (a * 16777619) seed


combineSeed : Seed -> Seed -> Seed
combineSeed a seed =
    Bit.xor a seed * 16777619
