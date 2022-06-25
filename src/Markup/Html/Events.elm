module Markup.Html.Events exposing
    ( onClick, onDoubleClick
    , onMouseDown, onMouseUp
    , onMouseEnter, onMouseLeave
    , onMouseOver, onMouseOut
    , onInput, onCheck, onSubmit
    , onBlur, onFocus
    , on, event
    )

{-|

@docs onClick, onDoubleClick
@docs onMouseDown, onMouseUp
@docs onMouseEnter, onMouseLeave
@docs onMouseOver, onMouseOut
@docs onInput, onCheck, onSubmit
@docs onBlur, onFocus
@docs on, event

-}

import Markup exposing (Attribute)
import Markup.Json.Encode exposing (MarkupValue)


onClick : MarkupValue -> Attribute
onClick =
    Markup.defineHtmlEvent "click" False False


onDoubleClick : MarkupValue -> Attribute
onDoubleClick =
    Markup.defineHtmlEvent "dblclick" False False


onMouseDown : MarkupValue -> Attribute
onMouseDown =
    Markup.defineHtmlEvent "mousedown" False False


onMouseUp : MarkupValue -> Attribute
onMouseUp =
    Markup.defineHtmlEvent "mouseup" False False


onMouseEnter : MarkupValue -> Attribute
onMouseEnter =
    Markup.defineHtmlEvent "mouseenter" False False


onMouseLeave : MarkupValue -> Attribute
onMouseLeave =
    Markup.defineHtmlEvent "mouseleave" False False


onMouseOver : MarkupValue -> Attribute
onMouseOver =
    Markup.defineHtmlEvent "mouseover" False False


onMouseOut : MarkupValue -> Attribute
onMouseOut =
    Markup.defineHtmlEvent "mouseout" False False


onInput : MarkupValue -> Attribute
onInput =
    Markup.defineHtmlEvent "input" True False


onCheck : MarkupValue -> Attribute
onCheck =
    Markup.defineHtmlEvent "change" False False


onSubmit : MarkupValue -> Attribute
onSubmit =
    Markup.defineHtmlEvent "submit" False True


onBlur : MarkupValue -> Attribute
onBlur =
    Markup.defineHtmlEvent "blur" False False


onFocus : MarkupValue -> Attribute
onFocus =
    Markup.defineHtmlEvent "focus" False False


on : String -> Bool -> Bool -> MarkupValue -> Attribute
on =
    Markup.htmlEvent


event : String -> Bool -> Bool -> MarkupValue -> Attribute
event =
    Markup.defineHtmlEvent
