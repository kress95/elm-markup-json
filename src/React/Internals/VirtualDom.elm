module React.Internals.VirtualDom exposing
    ( VirtualDom, text, node, nodeWithKey, encode
    , Prop, prop, createProp
    )

{-|

@docs VirtualDom, text, node, nodeWithKey, encode
@docs Prop, prop, createProp

-}

import Dict exposing (Dict)
import Html exposing (a)
import Json.Encode as Encode exposing (Value)
import React.Internals.Hash as Hash exposing (Hash)
import React.Internals.Json.HashedEncode as HashedEncode exposing (HashedValue)



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


createProp : String -> (a -> HashedValue) -> (a -> Prop)
createProp key encoder =
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
    if index > 0 && index < 10000 then
        case Dict.get index cache of
            Just k ->
                ( k, value )

            Nothing ->
                -- this should never happen
                ( HashedEncode.int index
                , value
                )

    else
        ( HashedEncode.int index
        , value
        )


cache : Dict Int HashedValue
cache =
    list 1000
        |> List.map (\i -> ( i, HashedEncode.int i ))
        |> Dict.fromList


list : Int -> List Int
list length =
    listHelp (length - 1) []


listHelp : Int -> List Int -> List Int
listHelp length xs =
    if length > -1 then
        listHelp (length - 1) (length :: xs)

    else
        xs


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
