module Markup.Html.Attributes exposing
    ( style, property, attribute, map
    , class, classList, id, title, hidden
    , type_, value, checked, placeholder, selected
    , accept, acceptCharset, action, autocomplete, autofocus
    , disabled, enctype, list, maxlength, minlength, method, multiple
    , name, novalidate, pattern, readonly, required, size, for, form
    , max, min, step
    , cols, rows, wrap
    , href, target, download, hreflang, media, ping, rel
    , ismap, usemap, shape, coords
    , src, height, width, alt
    , autoplay, controls, loop, preload, poster, default, kind, srclang
    , sandbox, srcdoc
    , reversed, start
    , align, colspan, rowspan, headers, scope
    , accesskey, contenteditable, contextmenu, dir, draggable, dropzone
    , itemprop, lang, spellcheck, tabindex
    , cite, datetime, pubdate, manifest
    )

{-|

@docs style, property, attribute, map
@docs class, classList, id, title, hidden
@docs type_, value, checked, placeholder, selected
@docs accept, acceptCharset, action, autocomplete, autofocus
@docs disabled, enctype, list, maxlength, minlength, method, multiple
@docs name, novalidate, pattern, readonly, required, size, for, form
@docs max, min, step
@docs cols, rows, wrap
@docs href, target, download, hreflang, media, ping, rel
@docs ismap, usemap, shape, coords
@docs src, height, width, alt
@docs autoplay, controls, loop, preload, poster, default, kind, srclang
@docs sandbox, srcdoc
@docs reversed, start
@docs align, colspan, rowspan, headers, scope
@docs accesskey, contenteditable, contextmenu, dir, draggable, dropzone
@docs itemprop, lang, spellcheck, tabindex
@docs cite, datetime, pubdate, manifest

-}

import Markup exposing (Attribute, key)
import Markup.Json.Encode as Encode exposing (MarkupValue)


style : List ( String, String ) -> Attribute
style =
    let
        attr =
            stringProperty "style"

        format =
            List.map (\( k, v ) -> k ++ ": " ++ v)
    in
    \styles -> attr (String.join ";" (format styles))


classList : List ( String, Bool ) -> Attribute
classList classes =
    class <|
        String.join " " <|
            List.map Tuple.first <|
                List.filter Tuple.second classes


property : String -> MarkupValue -> Attribute
property =
    Markup.at


intProperty : String -> Int -> Attribute
intProperty key =
    let
        attr =
            Markup.attribute key
    in
    \int -> attr (Encode.string (String.fromInt int))


stringProperty : String -> String -> Attribute
stringProperty key =
    let
        attr =
            Markup.attribute key
    in
    \string -> attr (Encode.string string)


boolProperty : String -> Bool -> Attribute
boolProperty key =
    let
        attr =
            Markup.attribute key
    in
    \bool -> attr (Encode.bool bool)


attribute : String -> String -> Attribute
attribute key string =
    Markup.at key (Encode.string string)


map : (a -> b) -> Attribute -> Attribute
map _ g =
    g


class : String -> Attribute
class =
    stringProperty "className"


hidden : Bool -> Attribute
hidden =
    boolProperty "hidden"


id : String -> Attribute
id =
    stringProperty "id"


title : String -> Attribute
title =
    stringProperty "title"


accesskey : Char -> Attribute
accesskey =
    let
        attr =
            stringProperty "accessKey"
    in
    \char -> attr (String.fromChar char)


contenteditable : Bool -> Attribute
contenteditable =
    boolProperty "contentEditable"


contextmenu : String -> Attribute
contextmenu =
    stringProperty "contextmenu"


dir : String -> Attribute
dir =
    stringProperty "dir"


draggable : String -> Attribute
draggable =
    stringProperty "draggable"


dropzone : String -> Attribute
dropzone =
    stringProperty "dropzone"


itemprop : String -> Attribute
itemprop =
    stringProperty "itemprop"


lang : String -> Attribute
lang =
    stringProperty "lang"


spellcheck : Bool -> Attribute
spellcheck =
    boolProperty "spellcheck"


tabindex : Int -> Attribute
tabindex =
    intProperty "tabIndex"


src : String -> Attribute
src =
    -- TODO: noJavaScriptOrHtmlUri
    stringProperty "src"


height : Int -> Attribute
height =
    intProperty "height"


width : Int -> Attribute
width =
    intProperty "width"


alt : String -> Attribute
alt =
    stringProperty "alt"


autoplay : Bool -> Attribute
autoplay =
    boolProperty "autoplay"


controls : Bool -> Attribute
controls =
    boolProperty "controls"


loop : Bool -> Attribute
loop =
    boolProperty "loop"


preload : String -> Attribute
preload =
    stringProperty "preload"


poster : String -> Attribute
poster =
    stringProperty "poster"


default : Bool -> Attribute
default =
    boolProperty "default"


kind : String -> Attribute
kind =
    stringProperty "kind"


srclang : String -> Attribute
srclang =
    stringProperty "srclang"


sandbox : String -> Attribute
sandbox =
    stringProperty "sandbox"


srcdoc : String -> Attribute
srcdoc =
    stringProperty "srcdoc"


type_ : String -> Attribute
type_ =
    stringProperty "type"


value : String -> Attribute
value =
    stringProperty "value"


checked : Bool -> Attribute
checked =
    boolProperty "checked"


placeholder : String -> Attribute
placeholder =
    stringProperty "placeholder"


selected : Bool -> Attribute
selected =
    boolProperty "selected"


accept : String -> Attribute
accept =
    stringProperty "accept"


acceptCharset : String -> Attribute
acceptCharset =
    stringProperty "acceptCharset"


action : String -> Attribute
action =
    -- TODO: noJavaScriptUri
    stringProperty "action"


autocomplete : Bool -> Attribute
autocomplete =
    let
        attr =
            stringProperty "autocomplete"
    in
    \bool ->
        attr
            (if bool then
                "on"

             else
                "off"
            )


autofocus : Bool -> Attribute
autofocus =
    boolProperty "autofocus"


disabled : Bool -> Attribute
disabled =
    boolProperty "disabled"


enctype : String -> Attribute
enctype =
    stringProperty "enctype"


list : String -> Attribute
list =
    stringProperty "list"


minlength : Int -> Attribute
minlength =
    intProperty "minLength"


maxlength : Int -> Attribute
maxlength =
    intProperty "maxlength"


method : String -> Attribute
method =
    stringProperty "method"


multiple : Bool -> Attribute
multiple =
    boolProperty "multiple"


name : String -> Attribute
name =
    stringProperty "name"


novalidate : Bool -> Attribute
novalidate =
    boolProperty "noValidate"


pattern : String -> Attribute
pattern =
    stringProperty "pattern"


readonly : Bool -> Attribute
readonly =
    boolProperty "readOnly"


required : Bool -> Attribute
required =
    boolProperty "required"


size : Int -> Attribute
size =
    intProperty "size"


for : String -> Attribute
for =
    stringProperty "htmlFor"


form : String -> Attribute
form =
    stringProperty "form"


max : String -> Attribute
max =
    stringProperty "max"


min : String -> Attribute
min =
    stringProperty "min"


step : String -> Attribute
step n =
    stringProperty "step" n


cols : Int -> Attribute
cols =
    intProperty "cols"


rows : Int -> Attribute
rows =
    intProperty "rows"


wrap : String -> Attribute
wrap =
    stringProperty "wrap"


ismap : Bool -> Attribute
ismap =
    boolProperty "isMap"


usemap : String -> Attribute
usemap =
    stringProperty "useMap"


shape : String -> Attribute
shape =
    stringProperty "shape"


coords : String -> Attribute
coords =
    stringProperty "coords"


align : String -> Attribute
align =
    stringProperty "align"


cite : String -> Attribute
cite =
    stringProperty "cite"


href : String -> Attribute
href =
    -- TODO: noJavaScriptUri
    stringProperty "href"


target : String -> Attribute
target =
    stringProperty "target"


download : String -> Attribute
download =
    stringProperty "download"


hreflang : String -> Attribute
hreflang =
    stringProperty "hreflang"


media : String -> Attribute
media =
    stringProperty "media"


ping : String -> Attribute
ping =
    stringProperty "ping"


rel : String -> Attribute
rel =
    stringProperty "rel"


datetime : String -> Attribute
datetime =
    stringProperty "datetime"


pubdate : String -> Attribute
pubdate =
    stringProperty "pubdate"


reversed : Bool -> Attribute
reversed =
    boolProperty "reversed"


start : Int -> Attribute
start =
    intProperty "start"


colspan : Int -> Attribute
colspan =
    intProperty "colspan"


headers : String -> Attribute
headers =
    stringProperty "headers"


rowspan : Int -> Attribute
rowspan =
    intProperty "rowspan"


scope : String -> Attribute
scope =
    stringProperty "scope"


manifest : String -> Attribute
manifest =
    stringProperty "manifest"
