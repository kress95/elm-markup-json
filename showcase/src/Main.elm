port module Main exposing (main)

import Action
import Effect exposing (Effect)
import Json.Decode as Decode exposing (Decoder, Value)
import Markup.Html as Html exposing (Html)
import Markup.Html.Events as Events
import Markup.Html.Lazy as Lazy exposing (Memo)



-- MODEL


type alias Model =
    { frames : Int
    , halves : Memo Int
    , seconds : Memo Html
    , clicks : Int
    , error : Maybe Decode.Error
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
            [ Html.p [] [ Html.text "half seconds (memo)" ]
            , Html.p [] [ Lazy.lazy model.halves ]
            ]
        , Html.div []
            [ Html.p [] [ Html.text "seconds (memoWith)" ]
            , Html.p [] [ Lazy.lazy model.seconds ]
            ]
        , Html.div [ Events.onClick (Action.example "EAE" False) ]
            [ Html.p [] [ Html.text "Click here" ]
            , if model.clicks > 0 then
                Html.p []
                    [ Html.text (String.fromInt model.clicks)
                    , if model.clicks > 1 then
                        Html.text " clicks"

                      else
                        Html.text " click"
                    ]

              else
                Html.div [] []
            ]
        , Html.div [ Events.onClick Action.clearError ]
            [ Html.p [] [ Html.text "Decode error: " ]
            , Html.p []
                [ Maybe.map Decode.errorToString model.error
                    |> Maybe.withDefault "(none)"
                    |> Html.text
                ]
            , Html.p [] [ Html.text "(click to clear)" ]
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
    | DecodeError Decode.Error
    | ClickedView
    | ClearError


expect : Decoder Msg
expect =
    Action.decoder
        { onExampleAction = \_ _ -> ClickedView
        , onClearError = ClearError
        }


init : Flags -> ( Model, Effect Msg )
init _ =
    ( { frames = 0
      , halves = Lazy.init viewHalves 0
      , seconds = Lazy.init someCrazyContainer (viewSeconds 0)
      , clicks = 0
      , error = Nothing
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
            ( { model
                | frames = frames
                , halves = Lazy.memo viewHalves (frames // 30) model.halves
                , seconds = Lazy.memoWith Html.isEqual someCrazyContainer (viewSeconds (frames // 60)) model.seconds
              }
            , Effect.delay 16 Interval
            )

        DecodeError error ->
            ( { model | error = Just error }, Effect.none )

        ClickedView ->
            ( { model | clicks = model.clicks + 1 }, Effect.none )

        ClearError ->
            ( { model | error = Nothing }, Effect.none )



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
        , expect = expect
        , onError = DecodeError
        }


port toHost : Value -> Cmd msg


port fromHost : (Value -> msg) -> Sub msg
