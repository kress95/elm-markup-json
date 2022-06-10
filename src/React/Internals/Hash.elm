module React.Internals.Hash exposing
    ( Hash, initialSeed
    , fromInt, fromIntWithSeed
    , fromString, fromStringWithSeed
    , combineAsymmetric, combineSymmetric
    )

{-| Mostly copied from <https://github.com/robinheghan/fnv1a>

@docs Hash, initialSeed
@docs fromInt, fromIntWithSeed
@docs fromString, fromStringWithSeed
@docs combineAsymmetric, combineSymmetric

-}

import Bitwise as Bit


type alias Hash =
    Int


fromInt : Int -> Hash
fromInt n =
    fromIntWithSeed n initialSeed


fromIntWithSeed : Int -> Hash -> Hash
fromIntWithSeed n seed =
    -- only well behaved with 32bit integers
    hash (Bit.and n 0xFF) seed
        |> hash (Bit.and (Bit.shiftRightBy n 8) 0xFF)
        |> hash (Bit.and (Bit.shiftRightBy n 16) 0xFF)
        |> hash (Bit.and (Bit.shiftRightBy n 24) 0xFF)


combineAsymmetric : Int -> Hash -> Hash
combineAsymmetric otherSeed seed =
    -- not sure how worse this is compared to the function above
    Bit.xor (otherSeed * 16777619) seed


combineSymmetric : Int -> Hash -> Hash
combineSymmetric otherSeed seed =
    -- not sure how worse this is compared to the function above
    Bit.xor otherSeed seed * 16777619


fromString : String -> Hash
fromString string =
    fromStringWithSeed string initialSeed


fromStringWithSeed : String -> Hash -> Hash
fromStringWithSeed str seed =
    Bit.shiftRightZfBy 0 (String.foldl utf32ToUtf8 seed str)


initialSeed : Hash
initialSeed =
    0x811C9DC5



-- internals


hash : Int -> Int -> Int
hash byte hashValue =
    {- Implementation ported from: https://gist.github.com/vaiorabbit/5657561 -}
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


utf32ToUtf8 : Char -> Int -> Int
utf32ToUtf8 char seed =
    {- Implementation copied from: https://github.com/zwilias/elm-utf-tools/tree/2.0.1 -}
    let
        byte =
            Char.toCode char
    in
    if byte < 0x80 then
        hash byte seed

    else if byte < 0x0800 then
        seed
            |> hash (Bit.or 0xC0 <| Bit.shiftRightZfBy 6 byte)
            |> hash (Bit.or 0x80 <| Bit.and 0x3F byte)

    else if byte < 0x00010000 then
        seed
            |> hash (Bit.or 0xE0 <| Bit.shiftRightZfBy 12 byte)
            |> hash (Bit.or 0x80 <| Bit.and 0x3F <| Bit.shiftRightZfBy 6 byte)
            |> hash (Bit.or 0x80 <| Bit.and 0x3F byte)

    else
        seed
            |> hash (Bit.or 0xF0 <| Bit.shiftRightZfBy 18 byte)
            |> hash (Bit.or 0x80 <| Bit.and 0x3F <| Bit.shiftRightZfBy 12 byte)
            |> hash (Bit.or 0x80 <| Bit.and 0x3F <| Bit.shiftRightZfBy 6 byte)
            |> hash (Bit.or 0x80 <| Bit.and 0x3F byte)
