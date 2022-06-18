module Json.HashEncode exposing
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
import React.Hash as Hash exposing (Hash)
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
    HashedValue (Hash.stringWith a stringSeed) (Encode.string a)


int : Int -> HashedValue
int a =
    HashedValue (Hash.intWith a intSeed) (Encode.int a)


float : Float -> HashedValue
float a =
    HashedValue (Hash.floatWith a floatSeed) (Encode.float a)


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
    listHelp arraySeed func entries


array : (value -> HashedValue) -> Array value -> HashedValue
array func entries =
    let
        values =
            Array.map func entries
    in
    HashedValue (Array.foldl hashValue arraySeed values)
        (Encode.array value values)


set : (value -> HashedValue) -> Set value -> HashedValue
set func entries =
    listHelp arraySeed func (Set.toList entries)


object : List ( String, HashedValue ) -> HashedValue
object pairs =
    HashedValue (List.foldl hashPair objectSeed pairs)
        (Encode.object (List.map unwrapValue pairs))


objectWithHash : String -> List ( String, HashedValue ) -> HashedValue
objectWithHash key pairs =
    let
        seed =
            List.foldl hashPair objectSeed pairs
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


stringSeed : Int
stringSeed =
    Hash.string "string"


intSeed : Int
intSeed =
    Hash.string "int"


floatSeed : Int
floatSeed =
    Hash.string "float"


arraySeed : Int
arraySeed =
    Hash.string "array"


objectSeed : Int
objectSeed =
    Hash.string "object"


hashValue : HashedValue -> Int -> Int
hashValue (HashedValue seed _) =
    Hash.join seed


hashPair : ( String, HashedValue ) -> Int -> Int
hashPair ( key, HashedValue valueHash _ ) seed =
    let
        keyHash =
            Hash.stringWith key stringSeed
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
        ( objectSeed, Dict.empty )
