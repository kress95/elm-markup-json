port module Main exposing (main)

import Json.Decode as Decode exposing (Value)
import Markup.Html as Html exposing (Html)
import Markup.Html.Lazy as Lazy exposing (Memo)
import Markup.Program exposing (Program, program)
import Process
import Task


type alias Model =
    { frames : Int
    , halves : Memo Int
    , seconds : Memo Html
    }


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
        , onError = always None
        }


init : {} -> ( Model, Cmd Msg )
init _ =
    ( { frames = 0
      , halves = Lazy.init viewHalves 0
      , seconds = Lazy.init someCrazyContainer (viewSeconds 0)
      }
    , delay 16 Interval
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )

        Interval ->
            let
                frames =
                    model.frames + 1
            in
            ( { frames = frames
              , halves = Lazy.memo viewHalves (frames // 30) model.halves
              , seconds = Lazy.memoWith Html.isEqual someCrazyContainer (viewSeconds (frames // 60)) model.seconds
              }
            , delay 16 Interval
            )


view : Model -> Html
view model =
    Html.div []
        [ Html.div []
            [ Html.p [] [ Html.text "frames" ]
            , Html.p [] [ Html.text (String.fromInt model.frames) ]
            ]
        , Html.div []
            [ Html.p [] [ Html.text "half seconds (memo)" ]
            , Html.p [] [ Lazy.lazy model.halves ]
            ]
        , Html.div []
            [ Html.p [] [ Html.text "seconds (memoWith)" ]
            , Html.p [] [ Lazy.lazy model.seconds ]
            ]
        ]


viewHalves : Int -> Html
viewHalves halves =
    Html.text (String.fromInt halves)


viewSeconds : Int -> Html
viewSeconds seconds =
    Html.text (String.fromInt seconds)


someCrazyContainer : Html -> Html
someCrazyContainer =
    List.singleton >> Html.div []


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


delay : Float -> msg -> Cmd msg
delay time msg =
    Task.perform (always msg) (Process.sleep time)


port toHost : Value -> Cmd msg


port fromHost : (Value -> msg) -> Sub msg
