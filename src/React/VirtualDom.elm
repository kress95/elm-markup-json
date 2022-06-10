module React.VirtualDom exposing (VirtualDom(..), encode, node, text)

import FNV1a
import Json.Encode as Encode exposing (Value)
import React.Json



-- TODO: hash props (maybe keep props/child hashes separated)


type VirtualDom
    = Text Int String
    | Node Int String React.Json.HashedValue React.Json.HashedValue Int (List VirtualDom)


text : String -> VirtualDom
text string =
    Text (FNV1a.hash string) string


node : String -> React.Json.HashedValue -> React.Json.HashedValue -> List VirtualDom -> VirtualDom
node tag props events children =
    let
        childrenHash =
            hashChildren children
    in
    Node (hashNode tag (React.Json.hash props) (React.Json.hash events) childrenHash)
        tag
        props
        events
        childrenHash
        children


encode : VirtualDom -> Value
encode tree =
    case tree of
        Text _ string ->
            Encode.string string

        Node _ tag props events _ children ->
            Encode.object
                [ ( "html", true )
                , ( "tag", Encode.string tag )
                , ( "props", React.Json.encode props )
                , ( "events", React.Json.encode events )
                , ( "children", Encode.list encode children )
                ]



-- internals


hashNode : String -> Int -> Int -> Int -> Int
hashNode tag props events children =
    FNV1a.hashWithSeed (String.fromInt events) children
        |> FNV1a.hashWithSeed (String.fromInt props)
        |> FNV1a.hashWithSeed tag


hashChildren : List VirtualDom -> Int
hashChildren =
    List.foldl hashItem 3


hashItem : VirtualDom -> Int -> Int
hashItem tree =
    FNV1a.hashWithSeed (String.fromInt (getHash tree))


getHash : VirtualDom -> Int
getHash tree =
    case tree of
        Text hash _ ->
            hash

        Node hash _ _ _ _ _ ->
            hash


true : Value
true =
    Encode.bool True
