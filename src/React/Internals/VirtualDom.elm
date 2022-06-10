module React.Internals.VirtualDom exposing (..)

import Json.Encode as Encode exposing (Value)
import React.Internals.Hash as Hash exposing (Hash)
import React.Internals.Json.EncodeHashed as EncodeHashed exposing (HashedValue)


type VirtualDom
    = VirtualDom HashedValue


text : String -> VirtualDom
text =
    EncodeHashed.string >> VirtualDom

node : List (String, HashedValue) -> List VirtualDom -> VirtualDom
node children =
    Debug.todo "a"



indexedNode : List (String, VirtualDom) -> VirtualDom
indexedNode children =
    EncodeHashed.list
        |> VirtualDom


encodeChild : (String, VirtualDom) -> VirtualDom
encodeChild ( key, (VirtualDom child) ) =
    EncodeHashed.object
        [ ( "k", EncodeHashed.string key )
        , ( "c", child )
        ]
        |> VirtualDom
