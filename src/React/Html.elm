module React.Html exposing
    ( Html, text, div, p, custom, customWithKey, encode
    , Attr, attr, customAttr
    )

{-|


# Html

@docs Html, text, div, p, custom, customWithKey, encode


# Attr

@docs Attr, attr, customAttr

-}

import Dict exposing (Dict)
import Json.Encode exposing (Value)
import React.VirtualDom as VirtualDom exposing (VirtualDom)
import React.Json.HashedEncode as HashedEncode exposing (HashedValue)



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
        tagAttr =
            toAttr tag
    in
    \attrs children -> VirtualDom.nodeWithKey (tagAttr :: attrs) (List.indexedMap withKey children)


customWithKey : String -> List Attr -> List ( HashedValue, VirtualDom ) -> VirtualDom
customWithKey tag =
    let
        tagAttr =
            toAttr tag
    in
    \attrs children -> VirtualDom.nodeWithKey (tagAttr :: attrs) children


encode : Html -> Value
encode =
    VirtualDom.encode



-- Attr


type alias Attr =
    VirtualDom.Prop


attr : String -> HashedValue -> Attr
attr =
    VirtualDom.prop


customAttr : String -> (a -> HashedValue) -> (a -> Attr)
customAttr =
    VirtualDom.customProp



-- internals


toAttr : String -> Attr
toAttr =
    customAttr "tag" HashedEncode.string


withKey : Int -> Html -> ( HashedValue, Html )
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
