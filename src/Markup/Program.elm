module Markup.Program exposing (PortsCmd, PortsMsg, PortsSub, Program, program)

import Json.Decode as Decode exposing (Decoder, Value)
import Markup exposing (Markup)
import Platform


type PortsModel model
    = Model Markup model


type PortsMsg msg
    = Msg msg
    | EventError Decode.Error


type alias PortsCmd msg =
    Cmd (PortsMsg msg)


type alias PortsSub msg =
    Sub (PortsMsg msg)


type alias Program flags model msg =
    Platform.Program flags (PortsModel model) (PortsMsg msg)


program :
    { init : flags -> ( model, Cmd msg )
    , view : model -> Markup
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , send : Value -> PortsCmd msg
    , receive : (Value -> PortsMsg msg) -> PortsSub msg
    , expect : Decoder msg
    }
    -> Program flags model msg
program config =
    Platform.worker
        { init = initFrom config
        , update = updateFrom config
        , subscriptions = subscriptionsFrom config
        }



-- internals


type alias Init a flags model msg =
    { a
        | init : flags -> ( model, Cmd msg )
        , view : model -> Markup
        , send : Value -> PortsCmd msg
    }


initFrom : Init a flags model msg -> flags -> ( PortsModel model, PortsCmd msg )
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
        , send (Markup.encode html)
        ]
    )


type alias Update a model msg =
    { a
        | update : msg -> model -> ( model, Cmd msg )
        , view : model -> Markup
        , send : Value -> PortsCmd msg
    }


updateFrom : Update a model msg -> PortsMsg msg -> PortsModel model -> ( PortsModel model, PortsCmd msg )
updateFrom { update, view, send } msg (Model html model) =
    let
        ( model_, cmd ) =
            case msg of
                Msg updateMsg ->
                    update updateMsg model

                EventError _ ->
                    -- TODO: create error page
                    ( model, Cmd.none )

        -- TODO only update on request animation frame
        html_ =
            view model_
    in
    ( Model html_ model_
    , if Markup.isEqual html html_ then
        Cmd.map Msg cmd

      else
        Cmd.batch
            [ Cmd.map Msg cmd
            , send (Markup.encode html_)
            ]
    )


type alias Subscriptions a model msg =
    { a
        | subscriptions : model -> Sub msg
        , receive : (Value -> PortsMsg msg) -> PortsSub msg
        , expect : Decoder msg
    }


subscriptionsFrom : Subscriptions a model msg -> PortsModel model -> PortsSub msg
subscriptionsFrom { subscriptions, receive, expect } (Model _ model) =
    Sub.batch
        [ Sub.map Msg (subscriptions model)
        , receive (subscriptionsDecode expect)
        ]


subscriptionsDecode : Decoder msg -> Value -> PortsMsg msg
subscriptionsDecode expect value =
    case Decode.decodeValue expect value of
        Ok msg ->
            Msg msg

        Err message ->
            EventError message
