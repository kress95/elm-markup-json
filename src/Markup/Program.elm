module Markup.Program exposing (MarkupCmd, MarkupSub, Msg, Program, program)

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Markup exposing (Markup, Seed)
import Platform


type State model
    = State (Internal model)


type alias Internal model =
    { model : model
    , dirty : Bool
    , hash : Seed
    }


type Msg msg
    = Msg msg
    | AnimationFrame


type alias MarkupCmd msg =
    Cmd (Msg msg)


type alias MarkupSub msg =
    Sub (Msg msg)


type alias Program flags model msg =
    Platform.Program flags (State model) (Msg msg)


program :
    { init : flags -> ( model, Cmd msg )
    , view : model -> Markup
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , send : Value -> MarkupCmd msg
    , receive : (Value -> Msg msg) -> MarkupSub msg
    , expect : Decoder msg
    , onError : Decode.Error -> msg
    }
    -> Program flags model msg
program config =
    let
        { init, view, update, subscriptions, send, receive, expect, onError } =
            config

        sendMarkup =
            Markup.encode >> send

        requestAnimationFrame =
            send Encode.null

        init_ flags =
            let
                ( model, cmd ) =
                    init flags

                markup =
                    view model
            in
            ( State { model = model, dirty = False, hash = Markup.hash markup }
            , Cmd.batch
                [ Cmd.map Msg cmd
                , sendMarkup markup
                ]
            )

        update_ msg (State ({ model, dirty } as state)) =
            case msg of
                Msg updateMsg ->
                    let
                        ( model_, cmd ) =
                            update updateMsg model

                        state_ =
                            State { state | model = model_, dirty = True }
                    in
                    if dirty then
                        ( state_
                        , Cmd.map Msg cmd
                        )

                    else
                        ( state_
                        , Cmd.batch
                            [ Cmd.map Msg cmd
                            , requestAnimationFrame
                            ]
                        )

                AnimationFrame ->
                    let
                        markup =
                            view model

                        hash =
                            Markup.hash markup

                        state_ =
                            State { state | hash = hash, dirty = False }
                    in
                    if hash == state.hash then
                        ( state_, Cmd.none )

                    else
                        ( state_, sendMarkup markup )

        subscriptions_ (State { model }) =
            Sub.batch
                [ Sub.map Msg (subscriptions model)
                , receive (decode expect onError)
                ]
    in
    Platform.worker
        { init = init_
        , update = update_
        , subscriptions = subscriptions_
        }



-- internals


decode : Decoder msg -> (Decode.Error -> msg) -> Value -> Msg msg
decode expect onError value =
    case Decode.decodeValue (decoder expect) value of
        Ok msg ->
            msg

        Err message ->
            Msg (onError message)


decoder : Decoder msg -> Decoder (Msg msg)
decoder expect =
    Decode.oneOf
        [ Decode.null AnimationFrame
        , Decode.map Msg expect
        ]
