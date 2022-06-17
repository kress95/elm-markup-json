module React.Internals.Json.HashedEncode exposing
    ( HashedValue, encode, hash, isEqual
    , string, int, float, bool, null
    , list, array, set
    , object, objectWithHash, dict
    , unsafeObject, unsafeObjectWithHash
    )

{-| Library for turning Elm values into Json values.


# Encoding

@docs HashedValue, encode, hash, value, isEqual


# Primitives

@docs string, int, float, bool, null


# Arrays

@docs list, array, set


# Objects

@docs object, objectWithHash, dict
@docs unsafeObject, unsafeObjectWithHash

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Encode as Encode exposing (Value)
import React.Internals.Hash as Hash exposing (Hash)
import Set exposing (Set)


type HashedValue
    = HashedValue Hash Value


hash : HashedValue -> Int
hash (HashedValue seed _) =
    seed


encode : HashedValue -> Value
encode (HashedValue _ a) =
    a


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
        (Encode.array encode values)


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


unsafeObject : (key -> ( Int, String )) -> List ( key, HashedValue ) -> HashedValue
unsafeObject getKey pairs =
    Encode.object (mapHelpUnsafeObject getKey unwrapUnsafeValue pairs)
        |> HashedValue (foldlHelpUnsafeObject getKey hashUnsafePair seedForObject pairs)


unsafeObjectWithHash : (key -> ( Int, String )) -> String -> List ( key, HashedValue ) -> HashedValue
unsafeObjectWithHash getKey key pairs =
    let
        seed =
            foldlHelpUnsafeObject getKey hashUnsafePair seedForObject pairs
    in
    Encode.object (( key, Encode.int seed ) :: mapHelpUnsafeObject getKey unwrapUnsafeValue pairs)
        |> HashedValue seed


dict : (k -> String) -> (v -> HashedValue) -> Dict k v -> HashedValue
dict toKey toValue dictionary =
    let
        ( seed, values ) =
            dictHelp toKey toValue dictionary
    in
    HashedValue seed (Encode.dict identity encode values)


isEqual : HashedValue -> HashedValue -> Bool
isEqual (HashedValue a _) (HashedValue b _) =
    a == b



-- internal


foldlHelpUnsafeObject : (a -> key) -> (( key, b ) -> value -> value) -> value -> List ( a, b ) -> value
foldlHelpUnsafeObject getKey func acc xs =
    case xs of
        [] ->
            acc

        ( key, value ) :: xs_ ->
            foldlHelpUnsafeObject getKey func (func ( getKey key, value ) acc) xs_


mapHelpUnsafeObject : (key -> b) -> (( b, a ) -> value) -> List ( key, a ) -> List value
mapHelpUnsafeObject getKey f xs =
    List.foldr (\( k, v ) acc -> f ( getKey k, v ) :: acc) [] xs


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
hashValue (HashedValue seed _) =
    Hash.join seed


hashPair : ( String, HashedValue ) -> Int -> Int
hashPair ( key, HashedValue valueHash _ ) seed =
    let
        keyHash =
            Hash.stringWith key seedForString
    in
    Hash.combine (Hash.join keyHash valueHash) seed


hashUnsafePair : ( ( Int, a ), HashedValue ) -> Int -> Int
hashUnsafePair ( ( keyHash, _ ), HashedValue valueHash _ ) seed =
    Hash.combine (Hash.join keyHash valueHash) seed


unwrapValue : ( String, HashedValue ) -> ( String, Value )
unwrapValue ( key, a ) =
    ( key, encode a )


unwrapUnsafeValue : ( ( a, String ), HashedValue ) -> ( String, Value )
unwrapUnsafeValue ( ( _, key ), a ) =
    ( key, encode a )


listHelp : Int -> (value -> HashedValue) -> List value -> HashedValue
listHelp seed func entries =
    let
        values =
            List.map func entries
    in
    HashedValue (List.foldl hashValue seed values) (Encode.list encode values)


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
        ( seedForDict, Dict.empty )
