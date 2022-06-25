module Markup exposing
    ( Markup, node, htmlNode, text, isEqual, encode
    , Seed, hash
    , Tag, tag
    , Key, key
    , Attribute, attribute, event, htmlEvent
    , defineAttribute, defineEvent, defineHtmlEvent
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

@docs Attribute, attribute, event, htmlEvent
@docs defineAttribute, defineEvent, defineHtmlEvent

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


htmlEvent : String -> Bool -> Bool -> MarkupValue -> Attribute
htmlEvent str stopPropagation preventDefault value =
    let
        valueSeed0 =
            joinSeed (HashEncode.hash value) seedForEvent

        valueSeed1 =
            if stopPropagation then
                joinSeed seedForStopPropagation valueSeed0

            else
                valueSeed0

        valueSeed2 =
            if preventDefault then
                joinSeed seedForPreventDefault valueSeed1

            else
                valueSeed1
    in
    Attribute
        (FNV1a.hashWithSeed str seedForAttribute)
        valueSeed2
        ( str
        , encodeHtmlEvent valueSeed2
            stopPropagation
            preventDefault
            (HashEncode.value value)
        )


defineHtmlEvent : String -> Bool -> Bool -> MarkupValue -> Attribute
defineHtmlEvent str =
    let
        keySeed =
            FNV1a.hashWithSeed str seedForAttribute
    in
    \stopPropagation preventDefault value ->
        let
            valueSeed0 =
                joinSeed (HashEncode.hash value) seedForEvent

            valueSeed1 =
                if stopPropagation then
                    joinSeed seedForStopPropagation valueSeed0

                else
                    valueSeed0

            valueSeed2 =
                if preventDefault then
                    joinSeed seedForPreventDefault valueSeed1

                else
                    valueSeed1
        in
        Attribute
            keySeed
            valueSeed2
            ( str
            , encodeHtmlEvent valueSeed2
                stopPropagation
                preventDefault
                (HashEncode.value value)
            )



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


encodeHtmlEvent : Seed -> Bool -> Bool -> Value -> Value
encodeHtmlEvent seed preventDefault stopPropagation context =
    let
        properties0 =
            [ ( "hash", Encode.int seed )
            , eventTag
            , ( "context", context )
            ]

        properties1 =
            if preventDefault then
                preventDefaultTag :: properties0

            else
                properties0
    in
    Encode.object
        (if stopPropagation then
            stopPropagationTag :: properties1

         else
            properties1
        )


eventTag : ( String, Value )
eventTag =
    ( "event", Encode.bool True )


preventDefaultTag : ( String, Value )
preventDefaultTag =
    ( "preventDefault", Encode.bool True )


stopPropagationTag : ( String, Value )
stopPropagationTag =
    ( "stopPropagation", Encode.bool True )


seedForPreventDefault : Seed
seedForPreventDefault =
    FNV1a.hash "preventDefault"


seedForStopPropagation : Seed
seedForStopPropagation =
    FNV1a.hash "stopPropagation"



--- general internals


joinSeed : Seed -> Seed -> Seed
joinSeed a seed =
    Bit.xor (a * 16777619) seed


combineSeed : Seed -> Seed -> Seed
combineSeed a seed =
    Bit.xor a seed * 16777619
