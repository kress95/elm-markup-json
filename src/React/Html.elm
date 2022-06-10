module React.Html exposing (Html, div, encode, p, text)

import Json.Encode
import React.Json
import React.VirtualDom as Tree exposing (VirtualDom)


type alias Html =
    VirtualDom


encode : Html -> Json.Encode.Value
encode =
    Tree.encode


text : String -> VirtualDom
text =
    Tree.text


div : List Html -> VirtualDom
div =
    Tree.node "div" React.Json.null React.Json.null


p : List Html -> VirtualDom
p =
    Tree.node "p" React.Json.null React.Json.null
