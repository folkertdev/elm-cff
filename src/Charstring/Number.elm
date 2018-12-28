module Charstring.Number exposing (Number(..), decode, decodeHelp, encode, fromInt, sizeFromFirstByte, toInt)

import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder)
import Bytes.Encode as Encode exposing (Encoder)


type Number
    = Fixed Int
    | Integer Int


fromInt : Int -> Number
fromInt =
    Integer


toInt : Number -> Int
toInt number =
    case number of
        Integer i ->
            i

        Fixed f ->
            round (toFloat f / 65536.0)


{-| The size of a Number in bytes

The size of a full number is known when reading its first byte

-}
sizeFromFirstByte : Int -> Int
sizeFromFirstByte byte =
    if byte == 28 then
        3

    else if byte == 255 then
        5

    else if byte >= 32 && byte <= 246 then
        1

    else if byte >= 247 && byte <= 254 then
        2

    else
        let
            _ =
                Debug.log "invalid byte" byte
        in
        Debug.todo "crash"


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
