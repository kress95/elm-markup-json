module React.Program exposing (Program, ProgramCmd, ProgramMsg, ProgramSub, worker)

import Json.Decode as Decode exposing (Decoder, Value)
import Platform
import React.Html as Html exposing (Html)
import React.Internals.VirtualDom as VirtualDom


type ProgramModel model
    = Model Html model


type ProgramMsg msg
    = Msg msg
    | EventError Decode.Error


type alias ProgramCmd msg =
    Cmd (ProgramMsg msg)


type alias ProgramSub msg =
    Sub (ProgramMsg msg)


type alias Config flags model msg =
    Init (Update (Subscriptions {} model msg) model msg) flags model msg


type alias Program flags model msg =
    Platform.Program flags (ProgramModel model) (ProgramMsg msg)


worker : Config flags model msg -> Program flags model msg
worker config =
    Platform.worker
        { init = initFrom config
        , update = updateFrom config
        , subscriptions = subscriptionsFrom config
        }



-- internals


type alias Init a flags model msg =
    { a
        | init : flags -> ( model, Cmd msg )
        , view : model -> Html
        , send : Value -> ProgramCmd msg
    }


initFrom : Init a flags model msg -> flags -> ( ProgramModel model, ProgramCmd msg )
initFrom { init, view, send } flags =
    let
        ( model, cmd ) =
            init flags

        html =
            view model
    in
    ( Model html model
    , Cmd.batch
        [ Cmd.map Msg cmd
        , send (Html.encode html)
        ]
    )


type alias Update a model msg =
    { a
        | update : msg -> model -> ( model, Cmd msg )
        , view : model -> Html
        , send : Value -> ProgramCmd msg
    }


updateFrom : Update a model msg -> ProgramMsg msg -> ProgramModel model -> ( ProgramModel model, ProgramCmd msg )
updateFrom { update, view, send } msg (Model html model) =
    let
        ( model_, cmd ) =
            case msg of
                Msg updateMsg ->
                    update updateMsg model

                EventError _ ->
                    -- TODO: create error page
                    ( model, Cmd.none )

        html_ =
            view model_
    in
    ( Model html_ model_
    , if VirtualDom.isEqual html html_ then
        Cmd.map Msg cmd

      else
        Cmd.batch
            [ Cmd.map Msg cmd
            , send (VirtualDom.encode html_)
            ]
    )


type alias Subscriptions a model msg =
    { a
        | subscriptions : model -> Sub msg
        , receive : (Value -> ProgramMsg msg) -> ProgramSub msg
        , expect : Decoder msg
    }


subscriptionsFrom : Subscriptions a model msg -> ProgramModel model -> ProgramSub msg
subscriptionsFrom { subscriptions, receive, expect } (Model _ model) =
    Sub.batch
        [ Sub.map Msg (subscriptions model)
        , receive (subscriptionsDecode expect)
        ]


subscriptionsDecode : Decoder msg -> Value -> ProgramMsg msg
subscriptionsDecode expect value =
    case Decode.decodeValue expect value of
        Ok msg ->
            Msg msg

        Err message ->
            EventError message
