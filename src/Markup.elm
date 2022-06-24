module Markup exposing
    ( Markup, node, tagNode, text, lazy, isEqual, encode
    , Seed, hash
    , Tag, tag
    , Key, key
    , Memo, memoInit, memo, memoWith
    , Attribute, at, ev, attribute, event
    )

{-|


# Markup

@docs Markup, node, tagNode, text, lazy, isEqual, encode
@docs Seed, hash
@docs Tag, tag
@docs Key, key


# Memo

@docs Memo, memoInit, memo, memoWith


# Attribute

@docs Attribute, at, ev, attribute, event

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


lazy : Memo value -> Markup
lazy (Memo _ markup) =
    markup


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



-- Memo


type Memo a
    = Memo a Markup


memoInit : (a -> Markup) -> a -> Memo a
memoInit view a =
    Memo a (view a)


memo : (a -> Markup) -> a -> Memo a -> Memo a
memo view a ((Memo b _) as memoized) =
    if a == b then
        memoized

    else
        Memo a (view a)


memoWith : (a -> a -> Bool) -> (a -> Markup) -> a -> Memo a -> Memo a
memoWith equals view a ((Memo b _) as memoized) =
    if equals a b then
        memoized

    else
        Memo a (view a)



-- Attributes


type Attribute
    = Attribute Seed Seed ( String, Value )


at : String -> MarkupValue -> Attribute
at str value =
    let
        valueSeed =
            HashEncode.hash value
    in
    Attribute
        (FNV1a.hashWithSeed str seedForAttribute)
        valueSeed
        ( str, encodeAttribute valueSeed (HashEncode.value value) )


ev : String -> MarkupValue -> Attribute
ev str value =
    let
        valueSeed =
            joinSeed (HashEncode.hash value) seedForEvent
    in
    Attribute
        (FNV1a.hashWithSeed str seedForAttribute)
        valueSeed
        ( str, encodeEvent valueSeed (HashEncode.value value) )


attribute : String -> MarkupValue -> Attribute
attribute str =
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
event str =
    -- TODO: prevent default / stop propagation
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
