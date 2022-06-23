module Markup.Html exposing
    ( Html, Tag, Key, text, div, p, custom, isEqual, encode
    , Attribute, attribute, event
    )

{-|


# Html

@docs Html, Tag, Key, text, div, p, custom, isEqual, encode


# Attribute

@docs Attribute, attribute, event

-}

import Dict exposing (Dict)
import Json.Encode exposing (Value)
import Markup.Json.Encode exposing (MarkupValue)
import Markup exposing (Markup)



-- Html


type alias Html =
    Markup


type alias Tag =
    Markup.Tag


type alias Key =
    Markup.Key


text : String -> Html
text =
    Markup.text


div : List Attribute -> List Html -> Html
div =
    custom (Markup.tag "div")


p : List Attribute -> List Html -> Html
p =
    custom (Markup.tag "p")


custom : Tag -> List Attribute -> List Html -> Html
custom tag attrs entries =
    Markup.tagNode tag attrs (List.indexedMap withKey entries)


encode : Html -> Value
encode =
    Markup.encode


isEqual : Html -> Html -> Bool
isEqual =
    Markup.isEqual



-- Attribute


type alias Attribute =
    Markup.Attribute


attribute : String -> MarkupValue -> Attribute
attribute =
    Markup.attribute


event : String -> MarkupValue -> Attribute
event =
    Markup.event



-- internals


withKey : Int -> Html -> ( Key, Html )
withKey index value =
    if index > 0 && index < 10000 then
        case Dict.get index cache of
            Just k ->
                ( k, value )

            Nothing ->
                -- this should never happen
                ( Markup.key (String.fromInt index)
                , value
                )

    else
        ( Markup.key (String.fromInt index)
        , value
        )


cache : Dict Int Key
cache =
    createIndex 1000
        |> List.map (\index -> ( index, Markup.key (String.fromInt index) ))
        |> Dict.fromList


createIndex : Int -> List Int
createIndex length =
    createIndexHelp (length - 1) []


createIndexHelp : Int -> List Int -> List Int
createIndexHelp length list =
    if length > -1 then
        createIndexHelp (length - 1) (length :: list)

    else
        list
