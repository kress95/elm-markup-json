module React.VirtualDom exposing (..)

{-|


# VirtualDom

@docs VirtualDom, text, node, nodeWithKey, isEqual, encode


# Prop

@docs Prop, prop, customProp

-}

-- ( VirtualDom, text, node, nodeWithKey, isEqual, encode
-- , Prop, prop, customProp
-- )

import Hash as Hash exposing (Hash)
import Json.Encode as Encode exposing (Value)
import Json.HashEncode as HashEncode exposing (HashedValue)



-- VirtualDom


type VirtualDom
    = VirtualDom ( Int, Value )


text : String -> VirtualDom
text txt =
    VirtualDom (HashEncode.unwrap (HashEncode.string txt))


node : List Prop -> List VirtualDom -> VirtualDom
node props children =
    nodeWithKey props (List.indexedMap withIndex children)


nodeWithKey : List Prop -> List ( HashedValue, VirtualDom ) -> VirtualDom
nodeWithKey props children =
    encodeNode
        [ propsEntry (encodeProps props)
        , childrenEntry (list encodeChild children)
        ]


isEqual : VirtualDom -> VirtualDom -> Bool
isEqual (VirtualDom ( a, _ )) (VirtualDom ( b, _ )) =
    a == b


encode : VirtualDom -> Value
encode (VirtualDom ( _, value )) =
    value



-- Prop


type Prop
    = Prop ObjectEntry


prop : String -> HashedValue -> Prop
prop key hashedValue =
    let
        ( hashForValue, value ) =
            HashEncode.unwrap hashedValue
    in
    { hashForKey = Hash.stringWith key propSeed
    , hashForValue = Hash.join  hashForValue
    , entry = ( key, Encode.object value )
    }
        |> Prop

attr : HashedValue -> HashedValue
attr value =
    HashEncode.object
        [ ("e$", HashEncode.bool False )
        , ("v$", value )
        ]

event : HashedValue -> HashedValue
event value =
    HashEncode.object
        [ ("e$", HashEncode.bool True )
        , ("v$", value )
        ]


customProp : String -> (a -> HashedValue) -> (a -> Prop)
customProp key encoder =
    let
        toEntry =
            objectEntry key
    in
    \value -> Prop (toEntry (encoder value))



-- internals


withIndex : Int -> VirtualDom -> ( HashedValue, VirtualDom )
withIndex index value =
    ( HashEncode.int index, value )


encodeProps : List Prop -> ( Hash, Value )
encodeProps pairs =
    let
        seed =
            List.foldl foldHashingProps propsSeed pairs
    in
    ( seed, Encode.object (( "h", Encode.int seed ) :: List.map unwrapProp pairs) )


foldHashingProps : Prop -> Int -> Int
foldHashingProps (Prop { hashForKey, hashForValue }) seed =
    Hash.combine (Hash.join hashForKey hashForValue) seed


propsSeed : Int
propsSeed =
    Hash.string "props"


unwrapProp : Prop -> ( String, Value )
unwrapProp (Prop { entry }) =
    entry


encodeChild : ( HashedValue, VirtualDom ) -> ( Hash, Value )
encodeChild ( key, VirtualDom pair ) =
    encodeChildWithKey
        [ keyEntry key
        , valueEntry pair
        ]


propSeed : Int
propSeed =
    Hash.string "prop"



-- super internal speed hacks


type alias ObjectEntry =
    { hashForKey : Hash
    , hashForValue : Hash
    , entry : ( String, Value )
    }


encodeChildWithKey : List ObjectEntry -> ( Hash, Value )
encodeChildWithKey pairs =
    ( List.foldl foldHashingEntries objectSeed pairs
    , Encode.object (List.map unwrapEntryHashedKey pairs)
    )


encodeNode : List ObjectEntry -> VirtualDom
encodeNode pairs =
    let
        seed =
            List.foldl foldHashingEntries objectSeed pairs
    in
    ( seed
    , Encode.object
        (( "h", Encode.int seed )
            :: List.map unwrapEntryHashedKey pairs
        )
    )
        |> VirtualDom


foldHashingEntries : ObjectEntry -> Int -> Int
foldHashingEntries { hashForKey, hashForValue } seed =
    Hash.combine (Hash.join hashForKey hashForValue) seed


objectSeed : Int
objectSeed =
    Hash.string "object"


arraySeed : Int
arraySeed =
    Hash.string "array"


unwrapEntryHashedKey : ObjectEntry -> ( String, Value )
unwrapEntryHashedKey { entry } =
    entry


keyEntry : HashedValue -> ObjectEntry
keyEntry =
    objectEntry "k$"


valueEntry : ( Hash, Value ) -> ObjectEntry
valueEntry =
    objectEntry2 "v$"


propsEntry : ( Hash, Value ) -> ObjectEntry
propsEntry =
    objectEntry2 "p$"


childrenEntry : ( Hash, Value ) -> ObjectEntry
childrenEntry =
    objectEntry2 "c$"


objectEntry : String -> HashedValue -> ObjectEntry
objectEntry key =
    let
        hashForKey =
            HashEncode.toHash (HashEncode.string key)
    in
    \value ->
        { hashForKey = hashForKey
        , hashForValue = HashEncode.toHash value
        , entry = ( key, HashEncode.toValue value )
        }


objectEntry2 : String -> ( Hash, Value ) -> ObjectEntry
objectEntry2 key =
    let
        hashForKey =
            HashEncode.toHash (HashEncode.string key)
    in
    \( hashForValue, value ) ->
        { hashForKey = hashForKey
        , hashForValue = hashForValue
        , entry = ( key, value )
        }


list : (value -> ( Hash, Value )) -> List value -> ( Hash, Value )
list func entries =
    listHelp arraySeed func entries


listHelp : Int -> (value -> ( Hash, Value )) -> List value -> ( Hash, Value )
listHelp seed func entries =
    let
        values =
            List.map func entries
    in
    ( List.foldl hashValue seed values
    , Encode.list toValue values
    )


hashValue : ( Hash, Value ) -> Int -> Int
hashValue ( seed, _ ) =
    Hash.join seed


toValue : ( Hash, Value ) -> Value
toValue ( _, value ) =
    value
