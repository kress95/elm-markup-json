module React.VirtualDom exposing
    ( VirtualDom, text, node, nodeWithKey, isEqual, encode
    , Prop, attr, event
    )

{-|


# VirtualDom

@docs VirtualDom, text, node, nodeWithKey, isEqual, encode


# Prop

@docs Prop, attr, event

-}

import Hash as Hash exposing (Hash)
import Json.Encode as Encode exposing (Value)
import Json.HashEncode as HashEncode exposing (HashedValue)



-- VirtualDom


type VirtualDom
    = VirtualDom HashValuePair


type alias HashValuePair =
    ( Hash, Value )


text : String -> VirtualDom
text txt =
    VirtualDom (HashEncode.unwrap (HashEncode.string txt))


node : List Prop -> List VirtualDom -> VirtualDom
node props children =
    nodeWithKey props (List.indexedMap withIndex children)


nodeWithKey : List Prop -> List ( HashedValue, VirtualDom ) -> VirtualDom
nodeWithKey props children =
    encodeObjectWithHash
        [ propsEntry (encodeProps props)
        , childrenEntry (list encodeChild children)
        ]
        |> VirtualDom


isEqual : VirtualDom -> VirtualDom -> Bool
isEqual (VirtualDom ( a, _ )) (VirtualDom ( b, _ )) =
    a == b


encode : VirtualDom -> Value
encode (VirtualDom ( _, value )) =
    value



-- Prop


type Prop
    = Prop CachedEntry


attr : String -> (a -> HashedValue) -> (a -> Prop)
attr name encoder =
    prop wrapAttr name encoder


event : String -> (a -> HashedValue) -> (a -> Prop)
event name encoder =
    prop wrapEvent name encoder



-- VirtualDom internals


withIndex : Int -> VirtualDom -> ( HashedValue, VirtualDom )
withIndex index value =
    ( HashEncode.int index, value )


propsEntry : HashValuePair -> CachedEntry
propsEntry =
    entryWith "p$"


childrenEntry : HashValuePair -> CachedEntry
childrenEntry =
    entryWith "c$"


encodeChild : ( HashedValue, VirtualDom ) -> HashValuePair
encodeChild ( key, VirtualDom pair ) =
    encodeObject
        [ keyEntry (HashEncode.unwrap key)
        , valueEntry pair
        ]


keyEntry : HashValuePair -> CachedEntry
keyEntry =
    entryWith "k$"


valueEntry : HashValuePair -> CachedEntry
valueEntry =
    entryWith "v$"



-- Props internals


encodeProps : List Prop -> HashValuePair
encodeProps pairs =
    let
        seed =
            List.foldl foldHashProps objectSeed pairs
    in
    ( seed, Encode.object (( "h", Encode.int seed ) :: List.map unwrapProp pairs) )


foldHashProps : Prop -> Int -> Int
foldHashProps (Prop { hashForKey, hashForValue }) seed =
    Hash.combine (Hash.join hashForKey hashForValue) seed


unwrapProp : Prop -> ( String, Value )
unwrapProp (Prop { entry }) =
    entry



-- Prop internals


prop : (HashedValue -> HashValuePair) -> String -> (a -> HashedValue) -> (a -> Prop)
prop wrap name encoder =
    let
        hashForKey =
            HashEncode.toHash (HashEncode.string name)
    in
    \value ->
        let
            ( hashForValue, entry ) =
                wrap (encoder value)
        in
        { hashForKey = hashForKey
        , hashForValue = hashForValue
        , entry = ( name, entry )
        }
            |> Prop


wrapAttr : HashedValue -> HashValuePair
wrapAttr value =
    encodeObject
        [ attrEntry
        , valueEntry (HashEncode.unwrap value)
        ]


attrEntry : CachedEntry
attrEntry =
    entryWith "e$" (HashEncode.unwrap (HashEncode.bool False))


wrapEvent : HashedValue -> HashValuePair
wrapEvent value =
    encodeObject
        [ eventEntry
        , valueEntry (HashEncode.unwrap value)
        ]


eventEntry : CachedEntry
eventEntry =
    entryWith "e$" (HashEncode.unwrap (HashEncode.bool True))



-- CachedEntry internals


type alias CachedEntry =
    { hashForKey : Hash
    , hashForValue : Hash
    , entry : ( String, Value )
    }


encodeObject : List CachedEntry -> HashValuePair
encodeObject pairs =
    ( List.foldl foldHashEntries objectSeed pairs
    , Encode.object (List.map getEntry pairs)
    )


encodeObjectWithHash : List CachedEntry -> HashValuePair
encodeObjectWithHash pairs =
    let
        seed =
            List.foldl foldHashEntries objectSeed pairs
    in
    ( seed
    , Encode.object (( "h$", Encode.int seed ) :: List.map getEntry pairs)
    )


foldHashEntries : CachedEntry -> Int -> Int
foldHashEntries { hashForKey, hashForValue } seed =
    Hash.combine (Hash.join hashForKey hashForValue) seed


objectSeed : Int
objectSeed =
    Hash.string "object"


getEntry : CachedEntry -> ( String, Value )
getEntry =
    .entry


entryWith : String -> HashValuePair -> CachedEntry
entryWith key =
    let
        hashForKey =
            HashEncode.toHash (HashEncode.string key)
    in
    \( hashForValue, value ) ->
        { hashForKey = hashForKey
        , hashForValue = hashForValue
        , entry = ( key, value )
        }



-- List HashValuePair internals


list : (value -> HashValuePair) -> List value -> HashValuePair
list func entries =
    listHelp arraySeed func entries


arraySeed : Int
arraySeed =
    Hash.string "array"


listHelp : Int -> (value -> HashValuePair) -> List value -> HashValuePair
listHelp seed func entries =
    let
        values =
            List.map func entries
    in
    ( List.foldl hashValue seed values
    , Encode.list toValue values
    )


hashValue : HashValuePair -> Int -> Int
hashValue ( seed, _ ) =
    Hash.join seed


toValue : HashValuePair -> Value
toValue ( _, value ) =
    value
