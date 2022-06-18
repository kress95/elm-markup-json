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

import Json.Encode as Encode exposing (Value)
import React.Hash as Hash exposing (Hash)
import React.Json.HashedEncode as HashedEncode exposing (HashedValue)



-- VirtualDom


type VirtualDom
    = VirtualDom HashedValue


text : String -> VirtualDom
text =
    HashedEncode.string >> VirtualDom


node : List Prop -> List VirtualDom -> VirtualDom
node props children =
    nodeWithKey props (List.indexedMap withKey children)


nodeWithKey : List Prop -> List ( HashedValue, VirtualDom ) -> VirtualDom
nodeWithKey props children =
    HashedEncode.objectWithHash "h"
        [ ( "p", encodeProps props )
        , ( "c", HashedEncode.list encodeChild children )
        ]
        |> VirtualDom


isEqual : VirtualDom -> VirtualDom -> Bool
isEqual (VirtualDom a) (VirtualDom b) =
    HashedEncode.isEqual a b


encode : VirtualDom -> Value
encode (VirtualDom value) =
    HashedEncode.value value



-- Prop


type Prop
    = Prop Hash ( String, Value )


prop : String -> HashedValue -> Prop
prop key value =
    Prop (Hash.join (Hash.stringWith key propSeed) (HashedEncode.hash value))
        ( key, HashedEncode.value value )


customProp : String -> (a -> HashedValue) -> (a -> Prop)
customProp key encoder =
    let
        keyHash =
            Hash.stringWith key propSeed
    in
    \a ->
        let
            value =
                encoder a
        in
        Prop (Hash.join keyHash (HashedEncode.hash value))
            ( key, HashedEncode.value value )



-- internals


withKey : Int -> VirtualDom -> ( HashedValue, VirtualDom )
withKey index value =
    ( HashedEncode.int index, value )


encodeProps : List Prop -> HashedValue
encodeProps props =
    let
        seed =
            List.foldl reduceHashProps propsSeed props
    in
    Encode.object (( "h", Encode.int seed ) :: List.map unwrapProp props)
        |> HashedEncode.unsafe seed


reduceHashProps : Prop -> Int -> Int
reduceHashProps (Prop hash _) seed =
    Hash.combine hash seed


propsSeed : Int
propsSeed =
    Hash.string "props"


unwrapProp : Prop -> ( String, Value )
unwrapProp (Prop _ kv) =
    kv


encodeChild : ( HashedValue, VirtualDom ) -> HashedValue
encodeChild ( key, VirtualDom b ) =
    HashedEncode.objectWithHash "h"
        [ ( "k", key )
        , ( "v", b )
        ]


propSeed : Int
propSeed =
    Hash.string "prop"
