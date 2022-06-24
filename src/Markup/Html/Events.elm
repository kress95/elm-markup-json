module Markup.Html.Events exposing
    ( onClick, onDoubleClick
    , onMouseDown, onMouseUp
    , onMouseEnter, onMouseLeave
    , onMouseOver, onMouseOut
    , onInput, onCheck, onSubmit
    , onBlur, onFocus
    , on
    )

{-|

@docs onClick, onDoubleClick
@docs onMouseDown, onMouseUp
@docs onMouseEnter, onMouseLeave
@docs onMouseOver, onMouseOut
@docs onInput, onCheck, onSubmit
@docs onBlur, onFocus
@docs on

-}

import Markup exposing (Attribute)
import Markup.Json.Encode exposing (MarkupValue)


onClick : MarkupValue -> Attribute
onClick =
    on "click"


onDoubleClick : MarkupValue -> Attribute
onDoubleClick =
    on "dblclick"


onMouseDown : MarkupValue -> Attribute
onMouseDown =
    on "mousedown"


onMouseUp : MarkupValue -> Attribute
onMouseUp =
    on "mouseup"


onMouseEnter : MarkupValue -> Attribute
onMouseEnter =
    on "mouseenter"


onMouseLeave : MarkupValue -> Attribute
onMouseLeave =
    on "mouseleave"


onMouseOver : MarkupValue -> Attribute
onMouseOver =
    on "mouseover"


onMouseOut : MarkupValue -> Attribute
onMouseOut =
    on "mouseout"


onInput : MarkupValue -> Attribute
onInput =
    on "input"


onCheck : MarkupValue -> Attribute
onCheck =
    on "change"


onSubmit : MarkupValue -> Attribute
onSubmit =
    on "submit"


onBlur : MarkupValue -> Attribute
onBlur =
    on "blur"


onFocus : MarkupValue -> Attribute
onFocus =
    on "focus"


on : String -> MarkupValue -> Attribute
on =
    Markup.event
