port module Main exposing (main)

import Json.Decode as Decode exposing (Value)
import Process
import Markup.Program exposing (Program, program)
import Markup.Html as Html exposing (Html)
import Task


type alias Model =
    { seconds : Int }


type Msg
    = None
    | Interval


main : Program {} Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , send = toHost
        , receive = fromHost
        , expect = Decode.succeed None
        }


init : {} -> ( Model, Cmd Msg )
init flags =
    ( { seconds = 0 }, delay 16 Interval )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )

        Interval ->
            ( { seconds = model.seconds + 1 }, delay 16 Interval )


view : Model -> Html
view model =
    Html.div []
        [ Html.p []
            [ Html.text "Abc"
            , Html.text "def"
            , Html.text "hij"
            , Html.p []
                [ Html.text "Abc"
                , Html.text "def"
                , Html.text "hij"
                , Html.p []
                    [ Html.text "Abc"
                    , Html.text "def"
                    , Html.text "hij"
                    ]
                ]
            ]
        , Html.p []
            [ Html.text "Abc"
            , Html.text "def"
            , Html.text "hij"
            ]
        , Html.p []
            [ Html.text (String.fromInt model.seconds)
            ]
        , Html.p []
            [ Html.text "Abc"
            , Html.text "def"
            , Html.text "hij"
            , Html.p []
                [ Html.text "Abc"
                , Html.text "def"
                , Html.text "hij"
                ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


delay : Float -> msg -> Cmd msg
delay time msg =
    Task.perform (always msg) (Process.sleep time)


port toHost : Value -> Cmd msg


port fromHost : (Value -> msg) -> Sub msg
