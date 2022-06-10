module React.Json exposing
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
import FNV1a
import Json.Encode as Encode exposing (Value)
import Set exposing (Set)


type HashedValue
    = Value Int Encode.Value


hash : HashedValue -> Int
hash (Value seed _) =
    seed


encode : HashedValue -> Encode.Value
encode (Value _ value) =
    value


string : String -> HashedValue
string value =
    Value (FNV1a.hashWithSeed value seedForString) (Encode.string value)


int : Int -> HashedValue
int value =
    Value (FNV1a.hashWithSeed (String.fromInt value) seedForInt) (Encode.int value)


float : Float -> HashedValue
float value =
    Value (FNV1a.hashWithSeed (String.fromFloat value) seedForFloat) (Encode.float value)


bool : Bool -> HashedValue
bool value =
    if value then
        true

    else
        false


true : HashedValue
true =
    Value (FNV1a.hash "true") (Encode.bool True)


false : HashedValue
false =
    Value (FNV1a.hash "false") (Encode.bool False)


null : HashedValue
null =
    Value (FNV1a.hash "null") Encode.null


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
    FNV1a.hash "string"


seedForInt : Int
seedForInt =
    FNV1a.hash "int"


seedForFloat : Int
seedForFloat =
    FNV1a.hash "float"


seedForList : Int
seedForList =
    FNV1a.hash "list"


seedForArray : Int
seedForArray =
    FNV1a.hash "array"


seedForSet : Int
seedForSet =
    FNV1a.hash "set"


seedForObject : Int
seedForObject =
    FNV1a.hash "object"


seedForDict : Int
seedForDict =
    FNV1a.hash "dict"


hashValue : HashedValue -> Int -> Int
hashValue value =
    FNV1a.hashWithSeed (String.fromInt (hash value))


hashPair : ( String, HashedValue ) -> Int -> Int
hashPair ( key, value ) seed =
    FNV1a.hashWithSeed key (FNV1a.hashWithSeed (String.fromInt (hash value)) seed)


unwrapValue : ( String, HashedValue ) -> ( String, Encode.Value )
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
            ( FNV1a.hashWithSeed key (FNV1a.hashWithSeed (String.fromInt (hash value)) seed)
            , Dict.insert key value dictionary
            )
        )
        ( seedForDict, Dict.empty )
