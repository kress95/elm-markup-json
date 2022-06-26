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
    Markup.defineEvent "onClick"


onDoubleClick : MarkupValue -> Attribute
onDoubleClick =
    Markup.defineEvent "onDoubleClick"


onMouseDown : MarkupValue -> Attribute
onMouseDown =
    Markup.defineEvent "onMouseDown"


onMouseUp : MarkupValue -> Attribute
onMouseUp =
    Markup.defineEvent "onMouseUp"


onMouseEnter : MarkupValue -> Attribute
onMouseEnter =
    Markup.defineEvent "onMouseEnter"


onMouseLeave : MarkupValue -> Attribute
onMouseLeave =
    Markup.defineEvent "onMouseLeave"


onMouseOver : MarkupValue -> Attribute
onMouseOver =
    Markup.defineEvent "onMouseOver"


onMouseOut : MarkupValue -> Attribute
onMouseOut =
    Markup.defineEvent "onMouseOut"


onInput : MarkupValue -> Attribute
onInput =
    custom "onInput" { preventDefault = False, stopPropagation = True }


onCheck : MarkupValue -> Attribute
onCheck =
    Markup.defineEvent "onChange"


onSubmit : MarkupValue -> Attribute
onSubmit =
    custom "onSubmit" { preventDefault = True, stopPropagation = False }


onBlur : MarkupValue -> Attribute
onBlur =
    Markup.defineEvent "onBlur"


onFocus : MarkupValue -> Attribute
onFocus =
    Markup.defineEvent "onFocus"


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
