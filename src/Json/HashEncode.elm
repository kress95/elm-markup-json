module Json.HashEncode exposing
    ( HashValue, hash, value, isEqual
    , string, int, float, bool, null
    , list, array, set
    , object, objectWithHash, dict
    )

{-| Library for turning Elm values into Json values.


# Encoding

@docs HashValue, hash, value, isEqual


# Primitives

@docs string, int, float, bool, null


# Arrays

@docs list, array, set


# Objects

@docs object, objectWithHash, dict

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Hash as Hash exposing (Hash)
import Json.Encode as Encode exposing (Value)
import Set exposing (Set)


type HashValue
    = HashValue Seed Value


type alias Seed =
    Int


hash : HashValue -> Hash
hash (HashValue seed _) =
    seed


value : HashValue -> Value
value (HashValue _ a) =
    a


isEqual : HashValue -> HashValue -> Bool
isEqual (HashValue a _) (HashValue b _) =
    a == b


string : String -> HashValue
string a =
    HashValue
        (Hash.stringWith a stringSeed)
        (Encode.string a)


int : Int -> HashValue
int a =
    HashValue
        (Hash.intWith a intSeed)
        (Encode.int a)


float : Float -> HashValue
float a =
    HashValue
        (Hash.floatWith a floatSeed)
        (Encode.float a)


bool : Bool -> HashValue
bool a =
    if a then
        true

    else
        false


true : HashValue
true =
    HashValue (Hash.string "true") (Encode.bool True)


false : HashValue
false =
    HashValue (Hash.string "false") (Encode.bool False)


null : HashValue
null =
    HashValue (Hash.string "null") Encode.null


list : (value -> HashValue) -> List value -> HashValue
list func entries =
    listHelp arraySeed func entries


array : (value -> HashValue) -> Array value -> HashValue
array func entries =
    let
        values =
            Array.map func entries
    in
    HashValue
        (Array.foldl hashValue arraySeed values)
        (Encode.array value values)


set : (value -> HashValue) -> Set value -> HashValue
set func entries =
    listHelp arraySeed func (Set.toList entries)


object : List ( String, HashValue ) -> HashValue
object pairs =
    HashValue
        (List.foldl hashPair objectSeed pairs)
        (Encode.object (List.map toEntry pairs))


objectWithHash : String -> List ( String, HashValue ) -> HashValue
objectWithHash key pairs =
    let
        seed =
            List.foldl hashPair objectSeed pairs
    in
    HashValue seed
        (Encode.object (( key, Encode.int seed ) :: List.map toEntry pairs))


dict : (k -> String) -> (v -> HashValue) -> Dict k v -> HashValue
dict toKey encode dictionary =
    let
        ( seed, values ) =
            dictHelp toKey encode dictionary
    in
    HashValue seed (Encode.dict identity value values)



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


hashValue : HashValue -> Int -> Int
hashValue (HashValue seed _) =
    Hash.join seed


hashPair : ( String, HashValue ) -> Int -> Int
hashPair ( key, HashValue valueHash _ ) seed =
    let
        keyHash =
            Hash.stringWith key stringSeed
    in
    Hash.combine (Hash.join keyHash valueHash) seed


toEntry : ( String, HashValue ) -> ( String, Value )
toEntry ( key, HashValue _ a ) =
    ( key, a )


listHelp : Int -> (value -> HashValue) -> List value -> HashValue
listHelp seed func entries =
    let
        values =
            List.map func entries
    in
    HashValue
        (List.foldl hashValue seed values)
        (Encode.list value values)


dictHelp : (k -> String) -> (v -> HashValue) -> Dict k v -> ( Int, Dict String HashValue )
dictHelp toKey encode =
    Dict.foldl
        (\k v ( seed, dictionary ) ->
            let
                key =
                    toKey k

                ((HashValue seed_ _) as a) =
                    encode v
            in
            ( Hash.stringWith key (Hash.combine seed_ seed)
            , Dict.insert key a dictionary
            )
        )
        ( objectSeed, Dict.empty )
