module Markup.Html.Keyed exposing
    ( Key, node
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

@docs Key, text, node
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

import Markup
import Markup.Html exposing (Attribute, Html)


type alias Key =
    Markup.Key


node : String -> List Attribute -> List ( Key, Html ) -> Html
node tag =
    Markup.htmlNode (Markup.tag tag)


section : List Attribute -> List ( Key, Html ) -> Html
section =
    node "section"


nav : List Attribute -> List ( Key, Html ) -> Html
nav =
    node "nav"


article : List Attribute -> List ( Key, Html ) -> Html
article =
    node "article"


aside : List Attribute -> List ( Key, Html ) -> Html
aside =
    node "aside"


h1 : List Attribute -> List ( Key, Html ) -> Html
h1 =
    node "h1"


h2 : List Attribute -> List ( Key, Html ) -> Html
h2 =
    node "h2"


h3 : List Attribute -> List ( Key, Html ) -> Html
h3 =
    node "h3"


h4 : List Attribute -> List ( Key, Html ) -> Html
h4 =
    node "h4"


h5 : List Attribute -> List ( Key, Html ) -> Html
h5 =
    node "h5"


h6 : List Attribute -> List ( Key, Html ) -> Html
h6 =
    node "h6"


header : List Attribute -> List ( Key, Html ) -> Html
header =
    node "header"


footer : List Attribute -> List ( Key, Html ) -> Html
footer =
    node "footer"


address : List Attribute -> List ( Key, Html ) -> Html
address =
    node "address"


main_ : List Attribute -> List ( Key, Html ) -> Html
main_ =
    node "main"


p : List Attribute -> List ( Key, Html ) -> Html
p =
    node "p"


hr : List Attribute -> List ( Key, Html ) -> Html
hr =
    node "hr"


pre : List Attribute -> List ( Key, Html ) -> Html
pre =
    node "pre"


blockquote : List Attribute -> List ( Key, Html ) -> Html
blockquote =
    node "blockquote"


ol : List Attribute -> List ( Key, Html ) -> Html
ol =
    node "ol"


ul : List Attribute -> List ( Key, Html ) -> Html
ul =
    node "ul"


li : List Attribute -> List ( Key, Html ) -> Html
li =
    node "li"


dl : List Attribute -> List ( Key, Html ) -> Html
dl =
    node "dl"


dt : List Attribute -> List ( Key, Html ) -> Html
dt =
    node "dt"


dd : List Attribute -> List ( Key, Html ) -> Html
dd =
    node "dd"


figure : List Attribute -> List ( Key, Html ) -> Html
figure =
    node "figure"


figcaption : List Attribute -> List ( Key, Html ) -> Html
figcaption =
    node "figcaption"


div : List Attribute -> List ( Key, Html ) -> Html
div =
    node "div"


a : List Attribute -> List ( Key, Html ) -> Html
a =
    node "a"


em : List Attribute -> List ( Key, Html ) -> Html
em =
    node "em"


strong : List Attribute -> List ( Key, Html ) -> Html
strong =
    node "strong"


small : List Attribute -> List ( Key, Html ) -> Html
small =
    node "small"


s : List Attribute -> List ( Key, Html ) -> Html
s =
    node "s"


cite : List Attribute -> List ( Key, Html ) -> Html
cite =
    node "cite"


q : List Attribute -> List ( Key, Html ) -> Html
q =
    node "q"


dfn : List Attribute -> List ( Key, Html ) -> Html
dfn =
    node "dfn"


abbr : List Attribute -> List ( Key, Html ) -> Html
abbr =
    node "abbr"


time : List Attribute -> List ( Key, Html ) -> Html
time =
    node "time"


code : List Attribute -> List ( Key, Html ) -> Html
code =
    node "code"


var : List Attribute -> List ( Key, Html ) -> Html
var =
    node "var"


samp : List Attribute -> List ( Key, Html ) -> Html
samp =
    node "samp"


kbd : List Attribute -> List ( Key, Html ) -> Html
kbd =
    node "kbd"


sub : List Attribute -> List ( Key, Html ) -> Html
sub =
    node "sub"


sup : List Attribute -> List ( Key, Html ) -> Html
sup =
    node "sup"


i : List Attribute -> List ( Key, Html ) -> Html
i =
    node "i"


b : List Attribute -> List ( Key, Html ) -> Html
b =
    node "b"


u : List Attribute -> List ( Key, Html ) -> Html
u =
    node "u"


mark : List Attribute -> List ( Key, Html ) -> Html
mark =
    node "mark"


ruby : List Attribute -> List ( Key, Html ) -> Html
ruby =
    node "ruby"


rt : List Attribute -> List ( Key, Html ) -> Html
rt =
    node "rt"


rp : List Attribute -> List ( Key, Html ) -> Html
rp =
    node "rp"


bdi : List Attribute -> List ( Key, Html ) -> Html
bdi =
    node "bdi"


bdo : List Attribute -> List ( Key, Html ) -> Html
bdo =
    node "bdo"


span : List Attribute -> List ( Key, Html ) -> Html
span =
    node "span"


br : List Attribute -> List ( Key, Html ) -> Html
br =
    node "br"


wbr : List Attribute -> List ( Key, Html ) -> Html
wbr =
    node "wbr"


ins : List Attribute -> List ( Key, Html ) -> Html
ins =
    node "ins"


del : List Attribute -> List ( Key, Html ) -> Html
del =
    node "del"


img : List Attribute -> List ( Key, Html ) -> Html
img =
    node "img"


iframe : List Attribute -> List ( Key, Html ) -> Html
iframe =
    node "iframe"


embed : List Attribute -> List ( Key, Html ) -> Html
embed =
    node "embed"


object : List Attribute -> List ( Key, Html ) -> Html
object =
    node "object"


param : List Attribute -> List ( Key, Html ) -> Html
param =
    node "param"


video : List Attribute -> List ( Key, Html ) -> Html
video =
    node "video"


audio : List Attribute -> List ( Key, Html ) -> Html
audio =
    node "audio"


source : List Attribute -> List ( Key, Html ) -> Html
source =
    node "source"


track : List Attribute -> List ( Key, Html ) -> Html
track =
    node "track"


canvas : List Attribute -> List ( Key, Html ) -> Html
canvas =
    node "canvas"


math : List Attribute -> List ( Key, Html ) -> Html
math =
    node "math"


table : List Attribute -> List ( Key, Html ) -> Html
table =
    node "table"


caption : List Attribute -> List ( Key, Html ) -> Html
caption =
    node "caption"


colgroup : List Attribute -> List ( Key, Html ) -> Html
colgroup =
    node "colgroup"


col : List Attribute -> List ( Key, Html ) -> Html
col =
    node "col"


tbody : List Attribute -> List ( Key, Html ) -> Html
tbody =
    node "tbody"


thead : List Attribute -> List ( Key, Html ) -> Html
thead =
    node "thead"


tfoot : List Attribute -> List ( Key, Html ) -> Html
tfoot =
    node "tfoot"


tr : List Attribute -> List ( Key, Html ) -> Html
tr =
    node "tr"


td : List Attribute -> List ( Key, Html ) -> Html
td =
    node "td"


th : List Attribute -> List ( Key, Html ) -> Html
th =
    node "th"


form : List Attribute -> List ( Key, Html ) -> Html
form =
    node "form"


fieldset : List Attribute -> List ( Key, Html ) -> Html
fieldset =
    node "fieldset"


legend : List Attribute -> List ( Key, Html ) -> Html
legend =
    node "legend"


label : List Attribute -> List ( Key, Html ) -> Html
label =
    node "label"


input : List Attribute -> List ( Key, Html ) -> Html
input =
    node "input"


button : List Attribute -> List ( Key, Html ) -> Html
button =
    node "button"


select : List Attribute -> List ( Key, Html ) -> Html
select =
    node "select"


datalist : List Attribute -> List ( Key, Html ) -> Html
datalist =
    node "datalist"


optgroup : List Attribute -> List ( Key, Html ) -> Html
optgroup =
    node "optgroup"


option : List Attribute -> List ( Key, Html ) -> Html
option =
    node "option"


textarea : List Attribute -> List ( Key, Html ) -> Html
textarea =
    node "textarea"


output : List Attribute -> List ( Key, Html ) -> Html
output =
    node "output"


progress : List Attribute -> List ( Key, Html ) -> Html
progress =
    node "progress"


meter : List Attribute -> List ( Key, Html ) -> Html
meter =
    node "meter"


details : List Attribute -> List ( Key, Html ) -> Html
details =
    node "details"


summary : List Attribute -> List ( Key, Html ) -> Html
summary =
    node "summary"


menuitem : List Attribute -> List ( Key, Html ) -> Html
menuitem =
    node "menuitem"


menu : List Attribute -> List ( Key, Html ) -> Html
menu =
    node "menu"
