module Charstring.Number exposing
    ( Number(..), fromInt, toInt
    , encode, decode
    , decodeHelp
    )

{-| Numbers used in the Charstring 2 spec

Numbers are used as arguments to operators. Because both are encoded as bytes, we need a mechanism to differentiate operators from arguments, while still allowing all numbers to be arguments
Operators are encoded as numbers 0..31. Anything above 31 is a number. The operator 28 also encoded a number (a 16-bit signed integer, to be precise).
To be able to use 0..31 as arguments too, all the arguments are shifted over in a clever way (see [the spec]() for details).

The arguments can span several bytes each, and can encode an Integer, or a 16.16 fixed-point number (16 digits before the decimal point, 16 after).
In further use, the fixed point numbers are rounded to integers.


## Number

@docs Number, fromInt, toInt


## Encoding and Decoding

@docs encode, decode

-}

import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder)
import Bytes.Encode as Encode exposing (Encoder)


type Number
    = Fixed Int
    | Integer Int


{-| Convert an integer to a `Number`
-}
fromInt : Int -> Number
fromInt =
    Integer


{-| Convert a `Number` to an integer. This rounds the `Fixed` numbers to the nearest integer
-}
toInt : Number -> Int
toInt number =
    case number of
        Integer i ->
            i

        Fixed f ->
            -- Fixed is a 16.16 format, so 16 bits after the radix point (i.e. the decimal dot '.')
            -- Here we divide by 2^16 and round to get the nearest integer.
            round (toFloat f / 65536.0)


{-| Decode a `Number`
-}
decode : Decoder Number
decode =
    Decode.andThen decodeHelp Decode.unsignedInt8


decodeHelp : Int -> Decoder Number
decodeHelp byte =
    case byte of
        28 ->
            Decode.signedInt16 BE
                |> Decode.map Integer

        255 ->
            Decode.unsignedInt32 BE
                |> Decode.map Fixed

        _ ->
            if byte >= 32 && byte <= 246 then
                Decode.succeed (Integer (byte - 139))

            else if byte >= 247 && byte <= 250 then
                Decode.unsignedInt8
                    |> Decode.map
                        (\low ->
                            let
                                high =
                                    (byte - 247) * 256
                            in
                            Integer (high + low + 108)
                        )

            else if byte >= 251 && byte <= 254 then
                Decode.unsignedInt8
                    |> Decode.map
                        (\low ->
                            let
                                high =
                                    (byte - 251) * 256
                            in
                            Integer (-high - low - 108)
                        )

            else
                -- unreachable, all of u8 is covered
                Decode.fail


{-| Encode an integer as a `Number` by performing the shifts discussed above
-}
encode : Int -> Encoder
encode number =
    if number >= -107 && number <= 107 then
        Encode.unsignedInt8 (number + 139)

    else if number >= 108 && number <= 1131 then
        let
            value =
                number - 108
        in
        Encode.sequence
            [ Encode.unsignedInt8 ((value // 256) + 247)
            , Encode.unsignedInt8 (value |> modBy 256)
            ]

    else if number >= -1131 && number <= -108 then
        let
            value =
                abs (number + 108)
        in
        Encode.sequence
            [ Encode.unsignedInt8 ((value // 256) + 251)
            , Encode.unsignedInt8 (value |> modBy 256)
            ]

    else
        Encode.sequence
            [ Encode.unsignedInt8 28
            , Encode.unsignedInt8 (Bitwise.shiftRightBy 8 number)
            , Encode.unsignedInt8 (Bitwise.and number 0xFF)
            ]
