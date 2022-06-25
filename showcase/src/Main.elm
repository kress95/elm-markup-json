port module Main exposing (main)

import Effect exposing (Effect)
import Json.Decode as Decode exposing (Value)
import Markup.Html as Html exposing (Html)
import Markup.Html.Events as Events
import Markup.Html.Lazy as Lazy exposing (Memo)



-- MODEL


type alias Model =
    { frames : Int
    , halves : Memo Int
    , seconds : Memo Html
    }



-- VIEW


view : Model -> Html
view model =
    Html.div []
        [ Html.div []
            [ Html.p [] [ Html.text "frames" ]
            , Html.p [] [ Html.text (String.fromInt model.frames) ]
            ]
        , Html.div []
            [ Html.p
                []
                [ Html.text "half seconds (memo)" ]
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



-- UPDATE


type alias Flags =
    {}


type Msg
    = None
    | Interval


init : Flags -> ( Model, Effect Msg )
init _ =
    ( { frames = 0
      , halves = Lazy.init viewHalves 0
      , seconds = Lazy.init someCrazyContainer (viewSeconds 0)
      }
    , Effect.delay 16 Interval
    )


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        None ->
            ( model, Effect.none )

        Interval ->
            let
                frames =
                    model.frames + 1
            in
            ( { frames = frames
              , halves = Lazy.memo viewHalves (frames // 30) model.halves
              , seconds = Lazy.memoWith Html.isEqual someCrazyContainer (viewSeconds (frames // 60)) model.seconds
              }
            , Effect.delay 16 Interval
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MAIN


main : Effect.Application Flags Model Msg
main =
    Effect.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , send = toHost
        , receive = fromHost
        , expect = Decode.succeed None
        , onError = always None
        }


port toHost : Value -> Cmd msg


port fromHost : (Value -> msg) -> Sub msg
