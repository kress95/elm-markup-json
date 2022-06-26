module Markup.Html.Events.Decode exposing
    ( onClick, onDoubleClick
    , onMouseDown, onMouseUp
    , onMouseEnter, onMouseLeave
    , onMouseOver, onMouseOut
    , onInput, onCheck, onSubmit
    , onBlur, onFocus
    )

{-|

@docs onClick, onDoubleClick
@docs onMouseDown, onMouseUp
@docs onMouseEnter, onMouseLeave
@docs onMouseOver, onMouseOut
@docs onInput, onCheck, onSubmit
@docs onBlur, onFocus

-}

import Json.Decode as Decode exposing (Decoder)


onClick : Decoder ()
onClick =
    eventType "click"


onDoubleClick : Decoder ()
onDoubleClick =
    eventType "dblclick"


onMouseDown : Decoder ()
onMouseDown =
    eventType "mousedown"


onMouseUp : Decoder ()
onMouseUp =
    eventType "mouseup"


onMouseEnter : Decoder ()
onMouseEnter =
    eventType "mouseenter"


onMouseLeave : Decoder ()
onMouseLeave =
    eventType "mouseleave"


onMouseOver : Decoder ()
onMouseOver =
    eventType "mouseover"


onMouseOut : Decoder ()
onMouseOut =
    eventType "mouseout"


onInput : Decoder ()
onInput =
    eventType "input"


onCheck : Decoder ()
onCheck =
    eventType "change"


onSubmit : Decoder ()
onSubmit =
    eventType "submit"


onBlur : Decoder ()
onBlur =
    eventType "blur"


onFocus : Decoder ()
onFocus =
    eventType "focus"



-- internals


eventType : String -> Decoder ()
eventType evType =
    Decode.andThen (eventTypeDecoder evType) (Decode.field "type" Decode.string)


eventTypeDecoder : String -> String -> Decoder ()
eventTypeDecoder evType ev =
    if ev == evType then
        Decode.succeed ()

    else
        Decode.fail ("Expected " ++ evType ++ " event type, but found " ++ ev ++ ".")
