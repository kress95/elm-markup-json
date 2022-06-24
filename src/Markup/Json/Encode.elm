module Markup.Json.Encode exposing
    ( MarkupValue, hash, value, isEqual
    , string, int, int32, int52, float, float64, bool, null
    , list, array, set
    , object, objectWithHash, dict
    )

{-| Library for turning Elm values into Json values.


# Encoding

@docs MarkupValue, hash, value, isEqual


# Primitives

@docs string, int, int32, int52, float, float64, bool, null


# Arrays

@docs list, array, set


# Objects

@docs object, objectWithHash, dict

-}

import Array exposing (Array)
import Bitwise as Bit
import Dict exposing (Dict)
import FNV1a
import Json.Encode as Encode exposing (Value)
import Set exposing (Set)


type MarkupValue
    = HashValue Seed Value


type alias Seed =
    Int


hash : MarkupValue -> Seed
hash (HashValue seed _) =
    seed


value : MarkupValue -> Value
value (HashValue _ a) =
    a


isEqual : MarkupValue -> MarkupValue -> Bool
isEqual (HashValue a _) (HashValue b _) =
    a == b


string : String -> MarkupValue
string a =
    HashValue (FNV1a.hashWithSeed a stringSeed)
        (Encode.string a)


int : Int -> MarkupValue
int a =
    HashValue (hashIntWithSeed a intSeed)
        (Encode.int a)


int32 : Int -> MarkupValue
int32 a =
    HashValue (hashInt32WithSeed a intSeed)
        (Encode.int a)


int52 : Int -> MarkupValue
int52 a =
    HashValue (FNV1a.hashWithSeed (String.fromInt a) intSeed)
        (Encode.int a)


float : Float -> MarkupValue
float a =
    HashValue (hashFloatWithSeed a floatSeed)
        (Encode.float a)


float64 : Float -> MarkupValue
float64 a =
    HashValue (FNV1a.hashWithSeed (String.fromFloat a) floatSeed)
        (Encode.float a)


bool : Bool -> MarkupValue
bool a =
    if a then
        true

    else
        false


true : MarkupValue
true =
    HashValue (FNV1a.hash "true") (Encode.bool True)


false : MarkupValue
false =
    HashValue (FNV1a.hash "false") (Encode.bool False)


null : MarkupValue
null =
    HashValue (FNV1a.hash "null") Encode.null


list : (value -> MarkupValue) -> List value -> MarkupValue
list func entries =
    listHelp arraySeed func entries


array : (value -> MarkupValue) -> Array value -> MarkupValue
array func entries =
    let
        values =
            Array.map func entries
    in
    HashValue (Array.foldl hashValue arraySeed values)
        (Encode.array value values)


set : (value -> MarkupValue) -> Set value -> MarkupValue
set func entries =
    listHelp arraySeed func (Set.toList entries)


object : List ( String, MarkupValue ) -> MarkupValue
object pairs =
    HashValue (List.foldl hashPair objectSeed pairs)
        (Encode.object (List.map toEntry pairs))


objectWithHash : String -> List ( String, MarkupValue ) -> MarkupValue
objectWithHash key pairs =
    let
        seed =
            List.foldl hashPair objectSeed pairs
    in
    HashValue seed
        (Encode.object (( key, Encode.int seed ) :: List.map toEntry pairs))


dict : (k -> String) -> (v -> MarkupValue) -> Dict k v -> MarkupValue
dict toKey encode dictionary =
    let
        ( seed, values ) =
            dictHelp toKey encode dictionary
    in
    HashValue seed (Encode.dict identity value values)



-- internal


stringSeed : Int
stringSeed =
    FNV1a.hash "string"


intSeed : Int
intSeed =
    FNV1a.hash "int"


floatSeed : Int
floatSeed =
    FNV1a.hash "float"


arraySeed : Int
arraySeed =
    FNV1a.hash "array"


objectSeed : Int
objectSeed =
    FNV1a.hash "object"


hashValue : MarkupValue -> Int -> Int
hashValue (HashValue seed _) =
    joinSeed seed


hashPair : ( String, MarkupValue ) -> Int -> Int
hashPair ( key, HashValue valueHash _ ) seed =
    let
        keyHash =
            FNV1a.hashWithSeed key stringSeed
    in
    combineSeed (joinSeed keyHash valueHash) seed


toEntry : ( String, MarkupValue ) -> ( String, Value )
toEntry ( key, HashValue _ a ) =
    ( key, a )


listHelp : Int -> (value -> MarkupValue) -> List value -> MarkupValue
listHelp seed func entries =
    let
        values =
            List.map func entries
    in
    HashValue (List.foldl hashValue seed values)
        (Encode.list value values)


dictHelp : (k -> String) -> (v -> MarkupValue) -> Dict k v -> ( Int, Dict String MarkupValue )
dictHelp toKey encode =
    Dict.foldl
        (\k v ( seed, dictionary ) ->
            let
                key =
                    toKey k

                ((HashValue seed_ _) as a) =
                    encode v
            in
            ( FNV1a.hashWithSeed key (combineSeed seed_ seed)
            , Dict.insert key a dictionary
            )
        )
        ( objectSeed, Dict.empty )


joinSeed : Seed -> Seed -> Seed
joinSeed a seed =
    Bit.xor (a * 16777619) seed


combineSeed : Seed -> Seed -> Seed
combineSeed a seed =
    Bit.xor a seed * 16777619


hashInt32WithSeed : Int -> Seed -> Seed
hashInt32WithSeed a seed =
    hasher (Bit.and a 0xFF) seed
        |> hasher (Bit.and (Bit.shiftRightBy a 8) 0xFF)
        |> hasher (Bit.and (Bit.shiftRightBy a 16) 0xFF)
        |> hasher (Bit.and (Bit.shiftRightBy a 24) 0xFF)


hashIntWithSeed : Int -> Seed -> Seed
hashIntWithSeed a seed =
    let
        x =
            abs (a + 4503599627370496)

        y =
            x // 2 ^ 32
    in
    hasher (shiftRightZfBy24 y) seed
        |> hasher (shiftRightZfBy24 (Bit.shiftLeftBy y 8))
        |> hasher (shiftRightZfBy24 (Bit.shiftLeftBy y 16))
        |> hasher (shiftRightZfBy24 (Bit.shiftLeftBy y 24))
        |> hasher (shiftRightZfBy24 x)
        |> hasher (shiftRightZfBy24 (Bit.shiftLeftBy x 8))
        |> hasher (shiftRightZfBy24 (Bit.shiftLeftBy x 16))
        |> hasher (shiftRightZfBy24 (Bit.shiftLeftBy x 24))


hashFloatWithSeed : Float -> Seed -> Seed
hashFloatWithSeed a seed =
    hashInt32WithSeed (round (a ^ 100000)) seed


shiftRightZfBy24 : Int -> Int
shiftRightZfBy24 x =
    Bit.shiftRightZfBy x 24


hasher : Int -> Int -> Int
hasher byte seed =
    -- copied from: https://github.com/robinheghan/fnv1a
    -- (implementation ported from: https://gist.github.com/vaiorabbit/5657561)
    let
        mixed =
            Bit.xor byte seed
    in
    mixed
        + Bit.shiftLeftBy 1 mixed
        + Bit.shiftLeftBy 4 mixed
        + Bit.shiftLeftBy 7 mixed
        + Bit.shiftLeftBy 8 mixed
        + Bit.shiftLeftBy 24 mixed
