module React exposing (Html, Program, ProgramCmd, ProgramMsg, ProgramSub, program)

import Json.Decode exposing (Decoder, Value)
import React.Html as React
import React.Program as React


type alias Html =
    React.Html


type alias Program flags model msg =
    React.Program flags model msg


type alias ProgramMsg msg =
    React.ProgramMsg msg


type alias ProgramCmd msg =
    React.ProgramCmd msg


type alias ProgramSub msg =
    React.ProgramSub msg


program :
    { init : flags -> ( model, Cmd msg )
    , view : model -> Html
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , send : Value -> ProgramCmd msg
    , receive : (Value -> ProgramMsg msg) -> ProgramSub msg
    , expect : Decoder msg
    }
    -> Program flags model msg
program =
    React.worker
