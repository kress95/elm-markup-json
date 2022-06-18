module React.Html exposing
    ( Html, text, div, p, custom, customWithKey, encode
    , Attr, prop, event
    )

{-|


# Html

@docs Html, text, div, p, custom, customWithKey, encode


# Attr

@docs Attr, prop, event

-}

import Dict exposing (Dict)
import Json.Encode exposing (Value)
import Json.HashEncode as HashEncode exposing (HashedValue)
import React.VirtualDom as VirtualDom exposing (VirtualDom)



-- Html


type alias Html =
    VirtualDom


text : String -> VirtualDom
text =
    VirtualDom.text


div : List Attr -> List VirtualDom -> VirtualDom
div =
    custom "div"


p : List Attr -> List VirtualDom -> VirtualDom
p =
    custom "p"


custom : String -> List Attr -> List VirtualDom -> VirtualDom
custom tag =
    let
        tagProp =
            taggedWith tag
    in
    \attrs children -> VirtualDom.nodeWithKey (tagProp :: attrs) (List.indexedMap withKey children)


customWithKey : String -> List Attr -> List ( HashedValue, VirtualDom ) -> VirtualDom
customWithKey tag =
    let
        tagProp =
            taggedWith tag
    in
    \attrs children -> VirtualDom.nodeWithKey (tagProp :: attrs) children


encode : Html -> Value
encode =
    VirtualDom.encode



-- Attr


type alias Attr =
    VirtualDom.Attr


prop : String -> (a -> HashedValue) -> (a -> Attr)
prop =
    VirtualDom.prop


event : String -> (a -> HashedValue) -> (a -> Attr)
event =
    VirtualDom.event



-- internals


taggedWith : String -> Attr
taggedWith =
    prop "tag" HashEncode.string


withKey : Int -> Html -> ( HashedValue, Html )
withKey index value =
    if index > 0 && index < 10000 then
        case Dict.get index cache of
            Just k ->
                ( k, value )

            Nothing ->
                -- this should never happen
                ( HashEncode.int index
                , value
                )

    else
        ( HashEncode.int index
        , value
        )


cache : Dict Int HashedValue
cache =
    list 1000
        |> List.map (\i -> ( i, HashEncode.int i ))
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
