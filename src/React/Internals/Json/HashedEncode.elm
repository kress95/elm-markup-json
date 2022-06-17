module React.Internals.Json.HashedEncode exposing
    ( HashedValue, unsafe, value, hash, isEqual
    , string, int, float, bool, null
    , list, array, set
    , object, objectWithHash, dict
    )

{-| Library for turning Elm values into Json values.


# Encoding

@docs HashedValue, unsafe, value, hash, isEqual


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
    = HashedValue Hash Value


unsafe : Hash -> Value -> HashedValue
unsafe =
    HashedValue


hash : HashedValue -> Int
hash (HashedValue seed _) =
    seed


value : HashedValue -> Value
value (HashedValue _ a) =
    a


isEqual : HashedValue -> HashedValue -> Bool
isEqual (HashedValue a _) (HashedValue b _) =
    a == b


string : String -> HashedValue
string a =
    HashedValue (Hash.stringWith a seedForString) (Encode.string a)


int : Int -> HashedValue
int a =
    HashedValue (Hash.intWith a seedForInt) (Encode.int a)


float : Float -> HashedValue
float a =
    HashedValue (Hash.floatWith a seedForFloat) (Encode.float a)


bool : Bool -> HashedValue
bool a =
    if a then
        true

    else
        false


true : HashedValue
true =
    HashedValue (Hash.string "true") (Encode.bool True)


false : HashedValue
false =
    HashedValue (Hash.string "false") (Encode.bool False)


null : HashedValue
null =
    HashedValue (Hash.string "null") Encode.null


list : (value -> HashedValue) -> List value -> HashedValue
list func entries =
    listHelp seedForArray func entries


array : (value -> HashedValue) -> Array value -> HashedValue
array func entries =
    let
        values =
            Array.map func entries
    in
    HashedValue (Array.foldl hashValue seedForArray values)
        (Encode.array value values)


set : (value -> HashedValue) -> Set value -> HashedValue
set func entries =
    listHelp seedForArray func (Set.toList entries)


object : List ( String, HashedValue ) -> HashedValue
object pairs =
    HashedValue (List.foldl hashPair seedForObject pairs)
        (Encode.object (List.map unwrapValue pairs))


objectWithHash : String -> List ( String, HashedValue ) -> HashedValue
objectWithHash key pairs =
    let
        seed =
            List.foldl hashPair seedForObject pairs
    in
    Encode.object (( key, Encode.int seed ) :: List.map unwrapValue pairs)
        |> HashedValue seed


dict : (k -> String) -> (v -> HashedValue) -> Dict k v -> HashedValue
dict toKey toValue dictionary =
    let
        ( seed, values ) =
            dictHelp toKey toValue dictionary
    in
    HashedValue seed (Encode.dict identity value values)



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


hashValue : HashedValue -> Int -> Int
hashValue (HashedValue seed _) =
    Hash.join seed


hashPair : ( String, HashedValue ) -> Int -> Int
hashPair ( key, HashedValue valueHash _ ) seed =
    let
        keyHash =
            Hash.stringWith key seedForString
    in
    Hash.combine (Hash.join keyHash valueHash) seed


unwrapValue : ( String, HashedValue ) -> ( String, Value )
unwrapValue ( key, a ) =
    ( key, value a )


listHelp : Int -> (value -> HashedValue) -> List value -> HashedValue
listHelp seed func entries =
    let
        values =
            List.map func entries
    in
    HashedValue (List.foldl hashValue seed values) (Encode.list value values)


dictHelp : (k -> String) -> (v -> HashedValue) -> Dict k v -> ( Int, Dict String HashedValue )
dictHelp toKey toValue =
    Dict.foldl
        (\k v ( seed, dictionary ) ->
            let
                key =
                    toKey k

                ((HashedValue seed_ _) as a) =
                    toValue v
            in
            ( Hash.stringWith key (Hash.combine seed_ seed)
            , Dict.insert key a dictionary
            )
        )
        ( seedForObject, Dict.empty )
