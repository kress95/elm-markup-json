module Markup.Html.Events.Decode exposing
    ( onClick, onDoubleClick
    , onMouseDown, onMouseUp
    , onMouseEnter, onMouseLeave
    , onMouseOver, onMouseOut
    , onInput, onCheck, onSubmit
    , onBlur, onFocus, onAny
    )

{-|

@docs onClick, onDoubleClick
@docs onMouseDown, onMouseUp
@docs onMouseEnter, onMouseLeave
@docs onMouseOver, onMouseOut
@docs onInput, onCheck, onSubmit
@docs onBlur, onFocus, onAny

-}

import Json.Decode as Decode exposing (Decoder)



onClick : Decoder msg -> Decoder msg
onClick =
    eventType "click"


onDoubleClick : Decoder msg -> Decoder msg
onDoubleClick =
    eventType "dblclick"



onMouseDown : Decoder msg -> Decoder msg
onMouseDown =
    eventType "mousedown"



onMouseUp : Decoder msg -> Decoder msg
onMouseUp =
    eventType "mouseup"



onMouseEnter : Decoder msg -> Decoder msg
onMouseEnter =
    eventType "mouseenter"



onMouseLeave : Decoder msg -> Decoder msg
onMouseLeave =
    eventType "mouseleave"



onMouseOver : Decoder msg -> Decoder msg
onMouseOver =
    eventType "mouseover"



onMouseOut : Decoder msg -> Decoder msg
onMouseOut =
    eventType "mouseout"



onInput : Decoder msg -> Decoder msg
onInput =
    eventType "input"



onCheck : Decoder msg -> Decoder msg
onCheck =
    eventType "change"



onSubmit : Decoder msg -> Decoder msg
onSubmit =
    eventType "submit"



onBlur : Decoder msg -> Decoder msg
onBlur =
    eventType "blur"



onFocus : Decoder msg -> Decoder msg
onFocus =
    eventType "focus"



onAny : Decoder msg -> Decoder msg
onAny =
    Decode.field "value"




-- internals


eventType : String -> Decoder msg -> Decoder msg
eventType event msgDecoder =
    Decode.at ["event", "type"] Decode.string
        |> Decode.andThen (eventTypeDecoder msgDecoder event)


eventTypeDecoder : Decoder msg -> String -> String -> Decoder msg
eventTypeDecoder msgDecoder force event =
    if event == force then
        Decode.field "value" msgDecoder

    else
        Decode.fail <|
            "I was expecting a " ++ force ++ " event, but found " ++ event ++ " instead."
