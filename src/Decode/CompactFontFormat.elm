module Decode.CompactFontFormat exposing (OffsetSize(..), card16, card8, offset, offsetSize, offsetSizeInBytes)

import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Bytes.Encode as Encode
import Decode.Extra exposing (andMap)


card8 : Decoder Int
card8 =
    Decode.unsignedInt8


card16 : Decoder Int
card16 =
    Decode.unsignedInt16 BE


type OffsetSize
    = Bytes1
    | Bytes2
    | Bytes3
    | Bytes4


offsetSizeInBytes s =
    case s of
        Bytes1 ->
            1

        Bytes2 ->
            2

        Bytes3 ->
            3

        Bytes4 ->
            4


offsetSize =
    Decode.unsignedInt8
        |> Decode.andThen
            (\size ->
                case size of
                    1 ->
                        Decode.succeed Bytes1

                    2 ->
                        Decode.succeed Bytes2

                    3 ->
                        Decode.succeed Bytes3

                    4 ->
                        Decode.succeed Bytes4

                    _ ->
                        let
                            _ =
                                Debug.log "invalid offset size " size
                        in
                        Decode.fail
            )


offset : OffsetSize -> Decoder Int
offset size =
    case size of
        Bytes1 ->
            Decode.unsignedInt8

        Bytes2 ->
            Decode.unsignedInt16 BE

        Bytes3 ->
            -- TODO check this logic
            Decode.succeed (\small large -> small + Bitwise.shiftLeftBy 16 large)
                |> andMap (Decode.unsignedInt16 BE)
                |> andMap Decode.unsignedInt8
                |> Decode.map (Debug.log "---------------> unchecked logic")

        Bytes4 ->
            Decode.unsignedInt32 BE
