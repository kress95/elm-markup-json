module Markup.Html exposing
    ( Html, Attribute, text, node, encode, isEqual
    , h1, h2, h3, h4, h5, h6
    , div, p, hr, pre, blockquote
    , span, a, code, em, strong, i, b, u, sub, sup, br
    , ol, ul, li, dl, dt, dd
    , img, iframe, canvas, math
    , form, input, textarea, button, select, option
    , section, nav, article, aside, header, footer, address, main_
    , figure, figcaption
    , table, caption, colgroup, col, tbody, thead, tfoot, tr, td, th
    , fieldset, legend, label, datalist, optgroup, output, progress, meter
    , audio, video, source, track
    , embed, object, param
    , ins, del
    , small, cite, dfn, abbr, time, var, samp, kbd, s, q
    , mark, ruby, rt, rp, bdi, bdo, wbr
    , details, summary, menuitem, menu
    )

{-|

@docs Html, Attribute, text, node, encode, isEqual
@docs h1, h2, h3, h4, h5, h6
@docs div, p, hr, pre, blockquote
@docs span, a, code, em, strong, i, b, u, sub, sup, br
@docs ol, ul, li, dl, dt, dd
@docs img, iframe, canvas, math
@docs form, input, textarea, button, select, option
@docs section, nav, article, aside, header, footer, address, main_
@docs figure, figcaption
@docs table, caption, colgroup, col, tbody, thead, tfoot, tr, td, th
@docs fieldset, legend, label, datalist, optgroup, output, progress, meter
@docs audio, video, source, track
@docs embed, object, param
@docs ins, del
@docs small, cite, dfn, abbr, time, var, samp, kbd, s, q
@docs mark, ruby, rt, rp, bdi, bdo, wbr
@docs details, summary, menuitem, menu

-}

import Dict exposing (Dict)
import Json.Encode exposing (Value)
import Markup


type alias Html =
    Markup.Markup


type alias Attribute =
    Markup.Attribute


node : String -> List Attribute -> List Html -> Html
node tag =
    let
        custom =
            Markup.htmlNode (Markup.tag tag)
    in
    \attrs entries -> custom attrs (List.indexedMap withKey entries)


text : String -> Html
text =
    Markup.text


encode : Html -> Value
encode =
    Markup.encode


isEqual : Html -> Html -> Bool
isEqual =
    Markup.isEqual


section : List Attribute -> List Html -> Html
section =
    node "section"


nav : List Attribute -> List Html -> Html
nav =
    node "nav"


article : List Attribute -> List Html -> Html
article =
    node "article"


aside : List Attribute -> List Html -> Html
aside =
    node "aside"


h1 : List Attribute -> List Html -> Html
h1 =
    node "h1"


h2 : List Attribute -> List Html -> Html
h2 =
    node "h2"


h3 : List Attribute -> List Html -> Html
h3 =
    node "h3"


h4 : List Attribute -> List Html -> Html
h4 =
    node "h4"


h5 : List Attribute -> List Html -> Html
h5 =
    node "h5"


h6 : List Attribute -> List Html -> Html
h6 =
    node "h6"


header : List Attribute -> List Html -> Html
header =
    node "header"


footer : List Attribute -> List Html -> Html
footer =
    node "footer"


address : List Attribute -> List Html -> Html
address =
    node "address"


main_ : List Attribute -> List Html -> Html
main_ =
    node "main"


p : List Attribute -> List Html -> Html
p =
    node "p"


hr : List Attribute -> List Html -> Html
hr =
    node "hr"


pre : List Attribute -> List Html -> Html
pre =
    node "pre"


blockquote : List Attribute -> List Html -> Html
blockquote =
    node "blockquote"


ol : List Attribute -> List Html -> Html
ol =
    node "ol"


ul : List Attribute -> List Html -> Html
ul =
    node "ul"


li : List Attribute -> List Html -> Html
li =
    node "li"


dl : List Attribute -> List Html -> Html
dl =
    node "dl"


dt : List Attribute -> List Html -> Html
dt =
    node "dt"


dd : List Attribute -> List Html -> Html
dd =
    node "dd"


figure : List Attribute -> List Html -> Html
figure =
    node "figure"


figcaption : List Attribute -> List Html -> Html
figcaption =
    node "figcaption"


div : List Attribute -> List Html -> Html
div =
    node "div"


a : List Attribute -> List Html -> Html
a =
    node "a"


em : List Attribute -> List Html -> Html
em =
    node "em"


strong : List Attribute -> List Html -> Html
strong =
    node "strong"


small : List Attribute -> List Html -> Html
small =
    node "small"


s : List Attribute -> List Html -> Html
s =
    node "s"


cite : List Attribute -> List Html -> Html
cite =
    node "cite"


q : List Attribute -> List Html -> Html
q =
    node "q"


dfn : List Attribute -> List Html -> Html
dfn =
    node "dfn"


abbr : List Attribute -> List Html -> Html
abbr =
    node "abbr"


time : List Attribute -> List Html -> Html
time =
    node "time"


code : List Attribute -> List Html -> Html
code =
    node "code"


var : List Attribute -> List Html -> Html
var =
    node "var"


samp : List Attribute -> List Html -> Html
samp =
    node "samp"


kbd : List Attribute -> List Html -> Html
kbd =
    node "kbd"


sub : List Attribute -> List Html -> Html
sub =
    node "sub"


sup : List Attribute -> List Html -> Html
sup =
    node "sup"


i : List Attribute -> List Html -> Html
i =
    node "i"


b : List Attribute -> List Html -> Html
b =
    node "b"


u : List Attribute -> List Html -> Html
u =
    node "u"


mark : List Attribute -> List Html -> Html
mark =
    node "mark"


ruby : List Attribute -> List Html -> Html
ruby =
    node "ruby"


rt : List Attribute -> List Html -> Html
rt =
    node "rt"


rp : List Attribute -> List Html -> Html
rp =
    node "rp"


bdi : List Attribute -> List Html -> Html
bdi =
    node "bdi"


bdo : List Attribute -> List Html -> Html
bdo =
    node "bdo"


span : List Attribute -> List Html -> Html
span =
    node "span"


br : List Attribute -> List Html -> Html
br =
    node "br"


wbr : List Attribute -> List Html -> Html
wbr =
    node "wbr"


ins : List Attribute -> List Html -> Html
ins =
    node "ins"


del : List Attribute -> List Html -> Html
del =
    node "del"


img : List Attribute -> List Html -> Html
img =
    node "img"


iframe : List Attribute -> List Html -> Html
iframe =
    node "iframe"


embed : List Attribute -> List Html -> Html
embed =
    node "embed"


object : List Attribute -> List Html -> Html
object =
    node "object"


param : List Attribute -> List Html -> Html
param =
    node "param"


video : List Attribute -> List Html -> Html
video =
    node "video"


audio : List Attribute -> List Html -> Html
audio =
    node "audio"


source : List Attribute -> List Html -> Html
source =
    node "source"


track : List Attribute -> List Html -> Html
track =
    node "track"


canvas : List Attribute -> List Html -> Html
canvas =
    node "canvas"


math : List Attribute -> List Html -> Html
math =
    node "math"


table : List Attribute -> List Html -> Html
table =
    node "table"


caption : List Attribute -> List Html -> Html
caption =
    node "caption"


colgroup : List Attribute -> List Html -> Html
colgroup =
    node "colgroup"


col : List Attribute -> List Html -> Html
col =
    node "col"


tbody : List Attribute -> List Html -> Html
tbody =
    node "tbody"


thead : List Attribute -> List Html -> Html
thead =
    node "thead"


tfoot : List Attribute -> List Html -> Html
tfoot =
    node "tfoot"


tr : List Attribute -> List Html -> Html
tr =
    node "tr"


td : List Attribute -> List Html -> Html
td =
    node "td"


th : List Attribute -> List Html -> Html
th =
    node "th"


form : List Attribute -> List Html -> Html
form =
    node "form"


fieldset : List Attribute -> List Html -> Html
fieldset =
    node "fieldset"


legend : List Attribute -> List Html -> Html
legend =
    node "legend"


label : List Attribute -> List Html -> Html
label =
    node "label"


input : List Attribute -> List Html -> Html
input =
    node "input"


button : List Attribute -> List Html -> Html
button =
    node "button"


select : List Attribute -> List Html -> Html
select =
    node "select"


datalist : List Attribute -> List Html -> Html
datalist =
    node "datalist"


optgroup : List Attribute -> List Html -> Html
optgroup =
    node "optgroup"


option : List Attribute -> List Html -> Html
option =
    node "option"


textarea : List Attribute -> List Html -> Html
textarea =
    node "textarea"


output : List Attribute -> List Html -> Html
output =
    node "output"


progress : List Attribute -> List Html -> Html
progress =
    node "progress"


meter : List Attribute -> List Html -> Html
meter =
    node "meter"


details : List Attribute -> List Html -> Html
details =
    node "details"


summary : List Attribute -> List Html -> Html
summary =
    node "summary"


menuitem : List Attribute -> List Html -> Html
menuitem =
    node "menuitem"


menu : List Attribute -> List Html -> Html
menu =
    node "menu"



-- internals


withKey : Int -> b -> ( Markup.Key, b )
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


cache : Dict Int Markup.Key
cache =
    createIndex 1000
        |> List.map (\index -> ( index, Markup.key (String.fromInt index) ))
        |> Dict.fromList


createIndex : number -> List number
createIndex length =
    createIndexHelp (length - 1) []


createIndexHelp : number -> List number -> List number
createIndexHelp length list =
    if length > -1 then
        createIndexHelp (length - 1) (length :: list)

    else
        list
