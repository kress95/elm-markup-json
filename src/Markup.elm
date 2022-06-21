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

import FNV1a
import Json.Encode as Encode exposing (Value)
import Json.HashEncode as HashEncode exposing (HashedValue)
import Markup.Hash as Hash exposing (Hash)



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

        hash =
            Hash.join entriesHash (Hash.join attrsHash tagHash)
    in
    ( hash
    , Encode.object
        [ ( "tag", tagValue )
        , ( "hash", Encode.int hash )
        , ( "attrsHash", Encode.int attrsHash )
        , ( "attrs", encodeAttributes attrs )
        , ( "entriesHash", Encode.int entriesHash )
        , ( "entries", encodeEntries entries )
        , ( "keyed", encodeKeyed entries )
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

        hash =
            Hash.join attrsHash entriesHash
    in
    ( hash
    , Encode.object
        [ ( "hash", Encode.int hash )
        , ( "attrsHash", Encode.int attrsHash )
        , ( "attrs", encodeAttributes attrs )
        , ( "entriesHash", Encode.int entriesHash )
        , ( "entries", encodeEntries entries )
        , ( "keyed", encodeKeyed entries )
        ]
    )
        |> Markup


text : String -> Markup
text str =
    Markup ( FNV1a.hashWithSeed str textSeed, Encode.string str )



-- Markup internals


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


encodeKeyed : List ( Key, Markup ) -> Value
encodeKeyed =
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



-- Attributes


type Attribute
    = Attribute Hash Hash ( String, Value )


at : String -> HashedValue -> Attribute
at str value =
    let
        valueHash =
            HashEncode.toHash value
    in
    Attribute
        (FNV1a.hashWithSeed str attributeSeed)
        valueHash
        ( str, encodeAttribute valueHash (HashEncode.toValue value) )


ev : String -> HashedValue -> Attribute
ev str value =
    let
        valueHash =
            Hash.join (HashEncode.toHash value) eventSeed
    in
    Attribute
        (FNV1a.hashWithSeed str attributeSeed)
        valueHash
        ( str, encodeEvent valueHash (HashEncode.toValue value) )


attribute : String -> HashedValue -> Attribute
attribute str =
    let
        keyHash =
            FNV1a.hashWithSeed str attributeSeed
    in
    \value ->
        let
            valueHash =
                HashEncode.toHash value
        in
        Attribute
            keyHash
            valueHash
            ( str, encodeAttribute valueHash (HashEncode.toValue value) )


event : String -> HashedValue -> Attribute
event str =
    let
        keyHash =
            FNV1a.hashWithSeed str attributeSeed
    in
    \value ->
        let
            valueHash =
                Hash.join (HashEncode.toHash value) eventSeed
        in
        Attribute
            keyHash
            valueHash
            ( str, encodeEvent valueHash (HashEncode.toValue value) )



-- Attributes internals


attributeSeed : Hash
attributeSeed =
    FNV1a.hash "attribute"


encodeAttribute : Hash -> Value -> Value
encodeAttribute hash context =
    Encode.object
        [ ( "hash", Encode.int hash )
        , attributeTag
        , ( "context", context )
        ]


attributeTag : ( String, Value )
attributeTag =
    ( "event", Encode.bool False )


eventSeed : Hash
eventSeed =
    FNV1a.hash "event"


encodeEvent : Hash -> Value -> Value
encodeEvent hash context =
    Encode.object
        [ ( "hash", Encode.int hash )
        , eventTag
        , ( "context", context )
        ]


eventTag : ( String, Value )
eventTag =
    ( "event", Encode.bool True )
