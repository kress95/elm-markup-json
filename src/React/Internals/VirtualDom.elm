module React.Internals.VirtualDom exposing
    ( VirtualDom, text, node, nodeWithKey, encode
    , Prop, prop
    , Key, key
    )

{-|

@docs VirtualDom, text, node, nodeWithKey, encode
@docs Prop, prop
@docs Key, key

-}

import Dict exposing (Dict)
import Json.Encode exposing (Value)
import React.Internals.Hash as Hash exposing (Hash)
import React.Internals.Json.HashedEncode as HashedEncode exposing (HashedValue)


type VirtualDom
    = VirtualDom HashedValue


type Prop
    = Prop ( Hash, String )


type Key
    = Key HashedValue


text : String -> VirtualDom
text =
    HashedEncode.string >> VirtualDom


prop : String -> Prop
prop name =
    Prop ( Hash.stringWith name propSeed, name )


key : String -> Key
key =
    HashedEncode.string >> Key


node : List ( Prop, HashedValue ) -> List VirtualDom -> VirtualDom
node props children =
    HashedEncode.objectWithHash "#"
        [ ( "&", HashedEncode.unsafeObjectWithHash unwrapProp "#" props )
        , ( "*", HashedEncode.list encodeChild (List.indexedMap getCachedKey children) )
        ]
        |> VirtualDom


nodeWithKey : List ( Prop, HashedValue ) -> List ( Key, VirtualDom ) -> VirtualDom
nodeWithKey props children =
    HashedEncode.objectWithHash "#"
        [ ( "&", HashedEncode.unsafeObjectWithHash unwrapProp "#" props )
        , ( "*", HashedEncode.list encodeChild children )
        ]
        |> VirtualDom


encode : VirtualDom -> Value
encode (VirtualDom value) =
    HashedEncode.encode value



-- internals


propSeed : Hash
propSeed =
    Hash.string "prop"


unwrapProp : Prop -> ( Int, String )
unwrapProp (Prop pair) =
    pair


encodeChild : ( Key, VirtualDom ) -> HashedValue
encodeChild ( Key a, VirtualDom b ) =
    HashedEncode.objectWithHash "#"
        [ ( "@", a )
        , ( "$", b )
        ]


getCachedKey : Int -> VirtualDom -> ( Key, VirtualDom )
getCachedKey index value =
    if index > 0 && index < 10000 then
        case Dict.get index cachedKeys of
            Just k ->
                ( k, value )

            Nothing ->
                ( Key (HashedEncode.int index)
                , value
                )

    else
        ( Key (HashedEncode.int index)
        , value
        )


cachedKeys : Dict Int Key
cachedKeys =
    list 1000
        |> List.map (\i -> ( i, Key (HashedEncode.int i) ))
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
