module Markup.Html.Program exposing (Command, Msg, Program, Subscription, program)

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Markup exposing (Seed)
import Markup.Html exposing (Html)
import Platform


type Model model
    = Model (Internal model)


type alias Internal model =
    { model : model
    , dirty : Bool
    , hash : Seed
    }


type Msg msg
    = Msg msg
    | AnimationFrame


type alias Command msg =
    Cmd (Msg msg)


type alias Subscription msg =
    Sub (Msg msg)


type alias Program flags model msg =
    Platform.Program flags (Model model) (Msg msg)


program :
    { init : flags -> ( model, Cmd msg )
    , view : model -> Html
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , send : Value -> Command msg
    , receive : (Value -> Msg msg) -> Subscription msg
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
            ( Model { model = model, dirty = False, hash = Markup.hash markup }
            , Cmd.batch
                [ Cmd.map Msg cmd
                , sendMarkup markup
                ]
            )

        update_ msg (Model ({ model, dirty } as state)) =
            case msg of
                Msg updateMsg ->
                    let
                        ( model_, cmd ) =
                            update updateMsg model

                        state_ =
                            Model { state | model = model_, dirty = True }
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
                            Model { state | hash = hash, dirty = False }
                    in
                    if hash == state.hash then
                        ( state_, Cmd.none )

                    else
                        ( state_, sendMarkup markup )

        subscriptions_ (Model { model }) =
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
