module Json.HashEncode exposing
    ( HashedValue, unwrap, toHash, toValue, isEqual
    , string, int, float, bool, null
    , list, array, set
    , object, objectWithHash, dict
    )

{-| Library for turning Elm values into Json values.


# Encoding

@docs HashedValue, unwrap, toHash, toValue, isEqual


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


type HashedValue
    = HashedValue ( Hash, Value )


unwrap : HashedValue -> ( Hash, Value )
unwrap (HashedValue pair) =
    pair


toHash : HashedValue -> Hash
toHash (HashedValue ( hash, _ )) =
    hash


toValue : HashedValue -> Value
toValue (HashedValue ( _, value )) =
    value


isEqual : HashedValue -> HashedValue -> Bool
isEqual (HashedValue ( a, _ )) (HashedValue ( b, _ )) =
    a == b


string : String -> HashedValue
string a =
    ( Hash.stringWith a stringSeed
    , Encode.string a
    )
        |> HashedValue


int : Int -> HashedValue
int a =
    ( Hash.intWith a intSeed, Encode.int a )
        |> HashedValue


float : Float -> HashedValue
float a =
    ( Hash.floatWith a floatSeed, Encode.float a )
        |> HashedValue


bool : Bool -> HashedValue
bool a =
    if a then
        true

    else
        false


true : HashedValue
true =
    ( Hash.string "true", Encode.bool True )
        |> HashedValue


false : HashedValue
false =
    ( Hash.string "false", Encode.bool False )
        |> HashedValue


null : HashedValue
null =
    ( Hash.string "null", Encode.null )
        |> HashedValue


list : (value -> HashedValue) -> List value -> HashedValue
list func entries =
    listHelp arraySeed func entries


array : (value -> HashedValue) -> Array value -> HashedValue
array func entries =
    let
        values =
            Array.map func entries
    in
    ( Array.foldl hashValue arraySeed values
    , Encode.array toValue values
    )
        |> HashedValue


set : (value -> HashedValue) -> Set value -> HashedValue
set func entries =
    listHelp arraySeed func (Set.toList entries)


object : List ( String, HashedValue ) -> HashedValue
object pairs =
    ( List.foldl hashPair objectSeed pairs
    , Encode.object (List.map toEntry pairs)
    )
        |> HashedValue


objectWithHash : String -> List ( String, HashedValue ) -> HashedValue
objectWithHash key pairs =
    let
        seed =
            List.foldl hashPair objectSeed pairs
    in
    ( seed
    , Encode.object (( key, Encode.int seed ) :: List.map toEntry pairs)
    )
        |> HashedValue


dict : (k -> String) -> (v -> HashedValue) -> Dict k v -> HashedValue
dict toKey encode dictionary =
    let
        ( seed, values ) =
            dictHelp toKey encode dictionary
    in
    ( seed, Encode.dict identity toValue values )
        |> HashedValue



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
hashValue (HashedValue ( seed, _ )) =
    Hash.join seed


hashPair : ( String, HashedValue ) -> Int -> Int
hashPair ( key, HashedValue ( valueHash, _ ) ) seed =
    let
        keyHash =
            Hash.stringWith key stringSeed
    in
    Hash.combine (Hash.join keyHash valueHash) seed


toEntry : ( String, HashedValue ) -> ( String, Value )
toEntry ( key, HashedValue ( _, a ) ) =
    ( key, a )


listHelp : Int -> (value -> HashedValue) -> List value -> HashedValue
listHelp seed func entries =
    let
        values =
            List.map func entries
    in
    ( List.foldl hashValue seed values
    , Encode.list toValue values
    )
        |> HashedValue


dictHelp : (k -> String) -> (v -> HashedValue) -> Dict k v -> ( Int, Dict String HashedValue )
dictHelp toKey encode =
    Dict.foldl
        (\k v ( seed, dictionary ) ->
            let
                key =
                    toKey k

                ((HashedValue ( seed_, _ )) as a) =
                    encode v
            in
            ( Hash.stringWith key (Hash.combine seed_ seed)
            , Dict.insert key a dictionary
            )
        )
        ( objectSeed, Dict.empty )
