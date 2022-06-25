module Markup.Html.Events exposing
    ( onClick, onDoubleClick
    , onMouseDown, onMouseUp
    , onMouseEnter, onMouseLeave
    , onMouseOver, onMouseOut
    , onInput, onCheck, onSubmit
    , onBlur, onFocus
    , on, preventDefaultOn, stopPropagationOn, custom
    )

{-|

@docs onClick, onDoubleClick
@docs onMouseDown, onMouseUp
@docs onMouseEnter, onMouseLeave
@docs onMouseOver, onMouseOut
@docs onInput, onCheck, onSubmit
@docs onBlur, onFocus
@docs on, preventDefaultOn, stopPropagationOn, custom

-}

import Markup exposing (Attribute)
import Markup.Json.Encode exposing (MarkupValue)


onClick : MarkupValue -> Attribute
onClick =
    Markup.defineEvent "click"


onDoubleClick : MarkupValue -> Attribute
onDoubleClick =
    Markup.defineEvent "dblclick"


onMouseDown : MarkupValue -> Attribute
onMouseDown =
    Markup.defineEvent "mousedown"


onMouseUp : MarkupValue -> Attribute
onMouseUp =
    Markup.defineEvent "mouseup"


onMouseEnter : MarkupValue -> Attribute
onMouseEnter =
    Markup.defineEvent "mouseenter"


onMouseLeave : MarkupValue -> Attribute
onMouseLeave =
    Markup.defineEvent "mouseleave"


onMouseOver : MarkupValue -> Attribute
onMouseOver =
    Markup.defineEvent "mouseover"


onMouseOut : MarkupValue -> Attribute
onMouseOut =
    Markup.defineEvent "mouseout"


onInput : MarkupValue -> Attribute
onInput =
    custom "input" { preventDefault = False, stopPropagation = True }


onCheck : MarkupValue -> Attribute
onCheck =
    Markup.defineEvent "change"


onSubmit : MarkupValue -> Attribute
onSubmit =
    custom "submit" { preventDefault = True, stopPropagation = False }


onBlur : MarkupValue -> Attribute
onBlur =
    Markup.defineEvent "blur"


onFocus : MarkupValue -> Attribute
onFocus =
    Markup.defineEvent "focus"


on : String -> MarkupValue -> Attribute
on =
    Markup.event


preventDefaultOn : String -> MarkupValue -> Attribute
preventDefaultOn name =
    Markup.defineHtmlEvent name True False


stopPropagationOn : String -> MarkupValue -> Attribute
stopPropagationOn name =
    Markup.defineHtmlEvent name False True


custom : String -> { preventDefault : Bool, stopPropagation : Bool } -> MarkupValue -> Attribute
custom name { preventDefault, stopPropagation } =
    Markup.defineHtmlEvent name preventDefault stopPropagation
