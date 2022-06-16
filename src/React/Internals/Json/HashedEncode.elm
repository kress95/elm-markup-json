module React.Internals.Json.HashedEncode exposing
    ( encode, hash
    , string, int, float, bool, null
    , list, array, set
    , object, objectWithHash, dict
    , HashedValue, isEqual
    )

{-| Library for turning Elm values into Json values.


# Encoding

@docs encode, hash, Value


# Primitives

@docs string, int, float, bool, null


# Arrays

@docs list, array, set


# Objects

@docs object, objectWithHash, dict

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Encode as Encode exposing (Value)
import React.Internals.Hash as Hash exposing (Hash)
import Set exposing (Set)


type HashedValue
    = Value Hash Value


hash : HashedValue -> Int
hash (Value seed _) =
    seed


encode : HashedValue -> Value
encode (Value _ value) =
    value


string : String -> HashedValue
string value =
    Value (Hash.stringWith value seedForString) (Encode.string value)


int : Int -> HashedValue
int value =
    Value (Hash.intWith value seedForInt) (Encode.int value)


float : Float -> HashedValue
float value =
    Value (Hash.floatWith value seedForFloat) (Encode.float value)


bool : Bool -> HashedValue
bool value =
    if value then
        true

    else
        false


true : HashedValue
true =
    Value (Hash.string "true") (Encode.bool True)


false : HashedValue
false =
    Value (Hash.string "false") (Encode.bool False)


null : HashedValue
null =
    Value (Hash.string "null") Encode.null


list : (value -> HashedValue) -> List value -> HashedValue
list func entries =
    listHelp seedForArray func entries


array : (value -> HashedValue) -> Array value -> HashedValue
array func entries =
    let
        values =
            Array.map func entries
    in
    Value (Array.foldl hashValue seedForArray values)
        (Encode.array encode values)


set : (value -> HashedValue) -> Set value -> HashedValue
set func entries =
    listHelp seedForArray func (Set.toList entries)


object : List ( String, HashedValue ) -> HashedValue
object pairs =
    Value (List.foldl hashPair seedForObject pairs)
        (Encode.object (List.map unwrapValue pairs))


objectWithHash : String -> List ( String, HashedValue ) -> HashedValue
objectWithHash key pairs =
    let
        seed =
            List.foldl hashPair seedForObject pairs
    in
    Value seed (Encode.object (List.map unwrapValue (( key, int seed ) :: pairs)))


dict : (k -> String) -> (v -> HashedValue) -> Dict k v -> HashedValue
dict toKey toValue dictionary =
    let
        ( seed, values ) =
            dictHelp toKey toValue dictionary
    in
    Value seed (Encode.dict identity encode values)


isEqual : HashedValue -> HashedValue -> Bool
isEqual (Value a _) (Value b _) =
    a == b



-- internal


seedForString : Int
seedForString =
    Hash.string "string"


seedForInt : Int
seedForInt =
    Hash.string "int"


seedForFloat : Int
seedForFloat =
    Hash.string "float"


seedForArray : Int
seedForArray =
    Hash.string "array"


seedForObject : Int
seedForObject =
    Hash.string "object"


seedForDict : Int
seedForDict =
    Hash.string "dict"


hashValue : HashedValue -> Int -> Int
hashValue (Value seed _) =
    Hash.join seed


hashPair : ( String, HashedValue ) -> Int -> Int
hashPair ( key, Value seed_ _ ) seed =
    Hash.stringWith key (Hash.join seed_ seed)


unwrapValue : ( String, HashedValue ) -> ( String, Value )
unwrapValue ( key, value ) =
    ( key, encode value )


listHelp : Int -> (value -> HashedValue) -> List value -> HashedValue
listHelp seed func entries =
    let
        values =
            List.map func entries
    in
    Value (List.foldl hashValue seed values) (Encode.list encode values)


dictHelp : (k -> String) -> (v -> HashedValue) -> Dict k v -> ( Int, Dict String HashedValue )
dictHelp toKey toValue =
    Dict.foldl
        (\k v ( seed, dictionary ) ->
            let
                key =
                    toKey k

                ((Value seed_ _) as value) =
                    toValue v
            in
            ( Hash.stringWith key (Hash.combine seed_ seed)
            , Dict.insert key value dictionary
            )
        )
        ( seedForDict, Dict.empty )
