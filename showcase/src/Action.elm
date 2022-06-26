module Action exposing
    ( Config, example, clearError
    , decoder
    )

{-|

@docs Config, example, clearError
@docs decoder

-}

import Json.Decode as Decode exposing (Decoder)
import Markup.Html.Events.Decode as EventsDecode
import Markup.Json.Encode as Encode exposing (MarkupValue)


type alias Config msg =
    { onExampleAction : String -> Bool -> msg
    , onClearError : msg
    }


example : String -> Bool -> MarkupValue
example str bool =
    encode ExampleAction
        [ ( "str", Encode.string str )
        , ( "bool", Encode.bool bool )
        ]


clearError : MarkupValue
clearError =
    encode ClearError []


decoder : Config msg -> Decoder msg
decoder config =
    withAction <|
        \action ->
            case action of
                ExampleAction ->
                    Decode.map2 config.onExampleAction
                        (Decode.field "str" Decode.string)
                        (Decode.field "bool" Decode.bool)
                        |> EventsDecode.onClick

                ClearError ->
                    Decode.succeed config.onClearError



-- internals


type Action
    = ExampleAction
    | ClearError


encode : Action -> List ( String, MarkupValue ) -> MarkupValue
encode tag xs =
    Encode.object (( "action", Encode.string (toString tag) ) :: xs)


toString : Action -> String
toString tag =
    case tag of
        ExampleAction ->
            "ExampleAction"

        ClearError ->
            "ClearError"


actionDecoder : Decoder Action
actionDecoder =
    withActionString <|
        \action ->
            case action of
                "ExampleAction" ->
                    Decode.succeed ExampleAction

                "ClearError" ->
                    Decode.succeed ClearError

                _ ->
                    Decode.fail <|
                        "Trying to action, but action type "
                            ++ action
                            ++ " is not supported."


withActionString : (String -> Decoder action) -> Decoder action
withActionString f =
    Decode.andThen f (Decode.at [ "value", "action" ] Decode.string)


withAction : (Action -> Decoder msg) -> Decoder msg
withAction f =
    Decode.andThen f actionDecoder
