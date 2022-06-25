module Effect exposing (Application, Effect, application, batch, delay, none, perform)

import Json.Decode as Decode exposing (Decoder, Value)
import Markup.Html exposing (Html)
import Markup.Html.Program as Program exposing (Program)
import Process
import Task


type Effect msg
    = None
    | Batch (List (Effect msg))
    | Delay Float msg


type alias Application flags model msg =
    Program flags model msg


application :
    { init : flags -> ( model, Effect msg )
    , update : msg -> model -> ( model, Effect msg )
    , view : model -> Html
    , subscriptions : model -> Sub msg
    , send : Value -> Program.Command msg
    , receive : (Value -> Program.Msg msg) -> Program.Subscription msg
    , expect : Decoder msg
    , onError : Decode.Error -> msg
    }
    -> Application flags model msg
application ({ init, update } as config) =
    Program.program
        { init = \flags -> perform (init flags)
        , view = config.view
        , update = \msg model -> perform (update msg model)
        , subscriptions = config.subscriptions
        , send = config.send
        , receive = config.receive
        , expect = config.expect
        , onError = config.onError
        }


perform : ( model, Effect msg ) -> ( model, Cmd msg )
perform ( model, effect ) =
    case effect of
        None ->
            ( model, Cmd.none )

        Batch effects ->
            List.foldl batchEffect ( model, [] ) effects
                |> Tuple.mapSecond Cmd.batch

        Delay time msg ->
            ( model, Task.perform (always msg) (Process.sleep time) )


none : Effect msg
none =
    None


batch : List (Effect msg) -> Effect msg
batch =
    Batch


delay : Float -> msg -> Effect msg
delay =
    Delay



-- internals


batchEffect : Effect msg -> ( model, List (Cmd msg) ) -> ( model, List (Cmd msg) )
batchEffect effect ( model, cmds ) =
    perform ( model, effect )
        |> Tuple.mapSecond (\cmd -> cmd :: cmds)
