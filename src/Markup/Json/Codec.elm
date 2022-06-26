module Markup.Json.Codec exposing
    ( Codec
    , string, int, float, bool, null
    , list, array, set, dict
    , nullable, maybe, oneOf
    , ObjectCodec, objectCodec, field, object
    , map, lazy, succeed, fail, andThen
    )

{-|

@docs Codec
@docs string, int, float, bool, null
@docs list, array, set, dict
@docs nullable, maybe, oneOf
@docs ObjectCodec, objectCodec, field, object
@docs map, lazy, succeed, fail, andThen

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Markup.Json.Encode as Encode exposing (MarkupValue)
import Set exposing (Set)


type Codec value
    = Codec (Internal value)


type alias Internal value =
    { encoder : value -> MarkupValue
    , decoder : Decoder value
    }


string : Codec String
string =
    Codec
        { encoder = Encode.string
        , decoder = Decode.string
        }


int : Codec Int
int =
    Codec
        { encoder = Encode.int
        , decoder = Decode.int
        }


float : Codec Float
float =
    Codec
        { encoder = Encode.float
        , decoder = Decode.float
        }


bool : Codec Bool
bool =
    Codec
        { encoder = Encode.bool
        , decoder = Decode.bool
        }


null : value -> Codec value
null a =
    Codec
        { encoder = always Encode.null
        , decoder = Decode.null a
        }


list : { encoder : value -> MarkupValue, decoder : Decoder value } -> Codec (List value)
list { encoder, decoder } =
    Codec
        { encoder = Encode.list encoder
        , decoder = Decode.list decoder
        }


array : { encoder : value -> MarkupValue, decoder : Decoder value } -> Codec (Array value)
array { encoder, decoder } =
    Codec
        { encoder = Encode.array encoder
        , decoder = Decode.array decoder
        }


set : { encoder : comparable -> MarkupValue, decoder : Decoder comparable } -> Codec (Set comparable)
set { encoder, decoder } =
    Codec
        { encoder = Encode.set encoder
        , decoder = Decode.map Set.fromList (Decode.list decoder)
        }


dict : { encoder : value -> MarkupValue, decoder : Decoder value } -> Codec (Dict String value)
dict { encoder, decoder } =
    Codec
        { encoder = Encode.dict identity encoder
        , decoder = Decode.dict decoder
        }


nullable : Codec a -> Codec (Maybe a)
nullable (Codec { encoder, decoder }) =
    Codec
        { decoder = Decode.nullable decoder
        , encoder =
            \v ->
                case v of
                    Nothing ->
                        Encode.null

                    Just x ->
                        encoder x
        }


maybe : Codec value -> Codec (Maybe value)
maybe (Codec { encoder, decoder }) =
    Codec
        { decoder = Decode.maybe decoder
        , encoder =
            \v ->
                case v of
                    Nothing ->
                        Encode.null

                    Just x ->
                        encoder x
        }


oneOf : Codec value -> List (Codec value) -> Codec value
oneOf (Codec { encoder, decoder }) alternatives =
    Codec
        { encoder = encoder
        , decoder = Decode.oneOf <| decoder :: List.map unwrapDecoder alternatives
        }


type ObjectCodec value next
    = ObjectCodec (ObjectCodecInternal value next)


type alias ObjectCodecInternal value constructor =
    { encoder_ : value -> List ( String, MarkupValue )
    , decoder_ : Decoder constructor
    }


objectCodec : constructor -> ObjectCodec value constructor
objectCodec ctor =
    ObjectCodec
        { encoder_ = \_ -> []
        , decoder_ = Decode.succeed ctor
        }


field : String -> (value -> field) -> Codec field -> ObjectCodec value (field -> constructor) -> ObjectCodec value constructor
field name get (Codec { encoder, decoder }) (ObjectCodec { encoder_, decoder_ }) =
    ObjectCodec
        { encoder_ = \v -> ( name, encoder (get v) ) :: encoder_ v
        , decoder_ = Decode.map2 (<|) decoder_ (Decode.field name decoder)
        }


object : ObjectCodec value value -> Codec value
object (ObjectCodec { encoder_, decoder_ }) =
    Codec
        { encoder = \v -> Encode.object (encoder_ v)
        , decoder = decoder_
        }


map : (a -> value) -> (value -> a) -> Codec a -> Codec value
map f g (Codec { encoder, decoder }) =
    Codec
        { decoder = Decode.map f decoder
        , encoder = \v -> encoder (g v)
        }


lazy : (() -> Codec value) -> Codec value
lazy f =
    Codec
        { decoder = Decode.lazy (\_ -> unwrapDecoder (f ()))
        , encoder = \v -> unwrapEncoder (f ()) v
        }


succeed : value -> Codec value
succeed value =
    Codec
        { decoder = Decode.succeed value
        , encoder = \_ -> Encode.null
        }


fail : String -> Codec value
fail message =
    Codec
        { decoder = Decode.fail message
        , encoder = always Encode.null
        }


andThen : (a -> Codec b) -> (b -> a) -> Codec a -> Codec b
andThen decode encode (Codec { encoder, decoder }) =
    Codec
        { decoder = decoder |> Decode.andThen (decode >> unwrapDecoder)
        , encoder = encoder << encode
        }



-- internals


unwrapDecoder : Codec value -> Decoder value
unwrapDecoder (Codec { decoder }) =
    decoder


unwrapEncoder : Codec value -> value -> MarkupValue
unwrapEncoder (Codec { encoder }) =
    encoder
