module React.Internals.Json.EncodeHashed exposing
    ( encode, hash
    , string, int, float, bool, null
    , list, array, set
    , object, dict
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

@docs object, dict

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
    Value (Hash.fromStringWithSeed value seedForString) (Encode.string value)


int : Int -> HashedValue
int value =
    Value (Hash.fromIntWithSeed value seedForInt) (Encode.int value)


float : Float -> HashedValue
float value =
    Value (Hash.fromStringWithSeed (String.fromFloat value) seedForFloat) (Encode.float value)


bool : Bool -> HashedValue
bool value =
    if value then
        true

    else
        false


true : HashedValue
true =
    Value (Hash.fromString "true") (Encode.bool True)


false : HashedValue
false =
    Value (Hash.fromString "false") (Encode.bool False)


null : HashedValue
null =
    Value (Hash.fromString "null") Encode.null


list : (value -> HashedValue) -> List value -> HashedValue
list func entries =
    listHelp seedForList func entries


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
    listHelp seedForSet func (Set.toList entries)


object : List ( String, HashedValue ) -> HashedValue
object pairs =
    Value (List.foldl hashPair seedForObject pairs)
        (Encode.object (List.map unwrapValue pairs))


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
    Hash.fromString "string"


seedForInt : Int
seedForInt =
    Hash.fromString "int"


seedForFloat : Int
seedForFloat =
    Hash.fromString "float"


seedForList : Int
seedForList =
    Hash.fromString "list"


seedForArray : Int
seedForArray =
    Hash.fromString "array"


seedForSet : Int
seedForSet =
    Hash.fromString "set"


seedForObject : Int
seedForObject =
    Hash.fromString "object"


seedForDict : Int
seedForDict =
    Hash.fromString "dict"


hashValue : HashedValue -> Int -> Int
hashValue (Value seed _) =
    Hash.combineAsymmetric seed


hashPair : ( String, HashedValue ) -> Int -> Int
hashPair ( key, Value seed_ _ ) seed =
    Hash.fromStringWithSeed key (Hash.combineAsymmetric seed_ seed)


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

                value =
                    toValue v
            in
            ( Hash.fromStringWithSeed key (Hash.combineAsymmetric (hash value) seed)
            , Dict.insert key value dictionary
            )
        )
        ( seedForDict, Dict.empty )
