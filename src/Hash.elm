module Hash exposing
    ( Hash, initialSeed
    , join, combine
    , string, stringWith
    , int, intWith
    , longerInt, longerIntWith
    , anyInt, anyIntWith
    , float, floatWith
    , anyFloat, anyFloatWith
    )

{-| FNV1a but with a encoder-like API.


# Basics

@docs Hash, initialSeed
@docs join, combine


# String

@docs string, stringWith


# Int

@docs int, intWith
@docs longerInt, longerIntWith
@docs anyInt, anyIntWith


# Float

@docs float, floatWith
@docs anyFloat, anyFloatWith

-}

import Bitwise as Bit
import FNV1a


type alias Hash =
    Int


initialSeed : Hash
initialSeed =
    FNV1a.initialSeed


join : Hash -> Hash -> Hash
join a seed =
    -- not sure how worse this is compared to the function above
    Bit.xor (a * 16777619) seed


combine : Hash -> Hash -> Hash
combine a seed =
    -- not sure how worse this is compared to the function above
    Bit.xor a seed * 16777619



-- String


string : String -> Hash
string a =
    stringWith a initialSeed


stringWith : String -> Hash -> Hash
stringWith a seed =
    FNV1a.hashWithSeed a seed



-- Int


int : Int -> Hash
int a =
    intWith a initialSeed


intWith : Int -> Hash -> Hash
intWith a seed =
    -- only well behaved with 32bit integers
    hasher (Bit.and a 0xFF) seed
        |> hasher (Bit.and (Bit.shiftRightBy a 8) 0xFF)
        |> hasher (Bit.and (Bit.shiftRightBy a 16) 0xFF)
        |> hasher (Bit.and (Bit.shiftRightBy a 24) 0xFF)



-- Longer Int


longerInt : Int -> Hash
longerInt a =
    longerIntWith a initialSeed


longerIntWith : Int -> Hash -> Hash
longerIntWith a seed =
    -- slower but range is (Number.MAX_SAFE_INTEGER + 1) / 2
    let
        --  a + ((Number.MAX_SAFE_INTEGER + 1) / 2)
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



-- Any Int


anyInt : Int -> Hash
anyInt a =
    anyIntWith a initialSeed


anyIntWith : Int -> Hash -> Hash
anyIntWith a seed =
    stringWith (String.fromInt a) seed



-- Float


float : Float -> Hash
float a =
    floatWith a initialSeed


floatWith : Float -> Hash -> Hash
floatWith a seed =
    -- faster but range is ((Number.MAX_SAFE_INTEGER + 1) / 2) / 100000
    longerIntWith (round (a ^ 100000)) seed



-- Any Float


anyFloat : Float -> Hash
anyFloat a =
    anyFloatWith a initialSeed


anyFloatWith : Float -> Hash -> Hash
anyFloatWith a seed =
    stringWith (String.fromFloat a) seed



-- Internals


shiftRightZfBy24 : Int -> Int
shiftRightZfBy24 x =
    Bit.shiftRightZfBy x 24


hasher : Int -> Int -> Int
hasher byte hashValue =
    -- copied from: https://github.com/robinheghan/fnv1a
    -- (implementation ported from: https://gist.github.com/vaiorabbit/5657561)
    let
        mixed =
            Bit.xor byte hashValue
    in
    mixed
        + Bit.shiftLeftBy 1 mixed
        + Bit.shiftLeftBy 4 mixed
        + Bit.shiftLeftBy 7 mixed
        + Bit.shiftLeftBy 8 mixed
        + Bit.shiftLeftBy 24 mixed
