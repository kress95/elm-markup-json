module React.VirtualDom exposing
    ( VirtualDom, text, node, nodeWithKey, isEqual, encode
    , Prop, prop, customProp
    )

{-|


# VirtualDom

@docs VirtualDom, text, node, nodeWithKey, isEqual, encode


# Prop

@docs Prop, prop, customProp

-}

import Hash as Hash exposing (Hash)
import Json.Encode as Encode exposing (Value)
import Json.HashEncode as HashEncode exposing (HashedValue)



-- VirtualDom


type VirtualDom
    = VirtualDom HashedValue


text : String -> VirtualDom
text =
    HashEncode.string >> VirtualDom


node : List Prop -> List VirtualDom -> VirtualDom
node props children =
    nodeWithKey props (List.indexedMap withIndex children)


nodeWithKey : List Prop -> List ( HashedValue, VirtualDom ) -> VirtualDom
nodeWithKey props children =
    encodeEntryHashedKey
        [ propsEntry (encodeProps props)
        , childrenEntry (HashEncode.list encodeChild children)
        ]
        |> VirtualDom


isEqual : VirtualDom -> VirtualDom -> Bool
isEqual (VirtualDom a) (VirtualDom b) =
    HashEncode.isEqual a b


encode : VirtualDom -> Value
encode (VirtualDom value) =
    HashEncode.value value



-- -- Prop


type Prop
    = Prop EntryHashedKey


prop : String -> HashedValue -> Prop
prop key value =
    Prop
        ( Hash.stringWith key propSeed
        , HashEncode.hash value
        , ( key, HashEncode.value value )
        )


customProp : String -> (a -> HashedValue) -> (a -> Prop)
customProp key encoder =
    let
        toEntry =
            entryHashedKey key
    in
    \value -> Prop (toEntry (encoder value))



-- internals


withIndex : Int -> VirtualDom -> ( HashedValue, VirtualDom )
withIndex index value =
    ( HashEncode.int index, value )


encodeProps : List Prop -> HashedValue
encodeProps pairs =
    let
        seed =
            List.foldl foldHashingProps propsSeed pairs
    in
    Encode.object (( "h", Encode.int seed ) :: List.map unwrapProp pairs)
        |> HashEncode.unsafe seed


foldHashingProps : Prop -> Int -> Int
foldHashingProps (Prop ( keyHash, valueHash, _ )) seed =
    Hash.combine (Hash.join keyHash valueHash) seed


propsSeed : Int
propsSeed =
    Hash.string "props"


unwrapProp : Prop -> ( String, Value )
unwrapProp (Prop ( _, _, kv )) =
    kv


encodeChild : ( HashedValue, VirtualDom ) -> HashedValue
encodeChild ( key, VirtualDom b ) =
    encodeEntry
        [ keyEntry key
        , valueEntry b
        ]


propSeed : Int
propSeed =
    Hash.string "prop"



-- super internal speed hacks


type alias EntryHashedKey =
    ( Hash, Hash, ( String, Value ) )


encodeEntry : List EntryHashedKey -> HashedValue
encodeEntry pairs =
    HashEncode.unsafe (List.foldl foldHashingEntries objectSeed pairs)
        (Encode.object (List.map unwrapEntryHashedKey pairs))


encodeEntryHashedKey : List EntryHashedKey -> HashedValue
encodeEntryHashedKey pairs =
    let
        seed =
            List.foldl foldHashingEntries objectSeed pairs
    in
    Encode.object (( "h", Encode.int seed ) :: List.map unwrapEntryHashedKey pairs)
        |> HashEncode.unsafe seed


foldHashingEntries : EntryHashedKey -> Int -> Int
foldHashingEntries ( keyHash, valueHash, ( _, value ) ) seed =
    Hash.combine (Hash.join keyHash valueHash) seed


objectSeed : Int
objectSeed =
    Hash.string "object"


unwrapEntryHashedKey : EntryHashedKey -> ( String, Value )
unwrapEntryHashedKey ( _, _, ( key, value ) ) =
    ( key, value )


keyEntry : HashedValue -> EntryHashedKey
keyEntry =
    entryHashedKey "k"


valueEntry : HashedValue -> EntryHashedKey
valueEntry =
    entryHashedKey "v"


propsEntry : HashedValue -> EntryHashedKey
propsEntry =
    entryHashedKey "p"


childrenEntry : HashedValue -> EntryHashedKey
childrenEntry =
    entryHashedKey "c"


entryHashedKey : String -> HashedValue -> EntryHashedKey
entryHashedKey str =
    let
        hashedKey =
            HashEncode.hash (HashEncode.string str)
    in
    \value -> ( hashedKey, HashEncode.hash value, ( str, HashEncode.value value ) )
