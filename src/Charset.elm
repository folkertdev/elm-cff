module Charset exposing (Charset, decode, get)

import Array exposing (Array)
import Bytes exposing (Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Utils


type Charset
    = Format0 Format0Charset
    | Format1 Format1Charset
    | Format2 Format2Charset


type alias Format0Charset =
    Array Int


type alias Format1Charset =
    -- where .nLeft : u8
    Array Range


type alias Format2Charset =
    -- where .nLeft : u16
    Array Range


type alias Range =
    { first : Int, nLeft : Int }


decode charset nGlyphs =
    Decode.unsignedInt8
        |> Decode.andThen
            (\format ->
                case format of
                    0 ->
                        decodeFormat0 nGlyphs
                            |> Decode.map Format0

                    1 ->
                        decodeFormat1 nGlyphs
                            |> Decode.map Format1

                    _ ->
                        Decode.fail
            )


decodeFormat0 : Int -> Decoder Format0Charset
decodeFormat0 nGlyphs =
    Utils.exactly (nGlyphs - 1) (Decode.unsignedInt16 BE)
        |> Decode.map Array.fromList


decodeFormat1 : Int -> Decoder Format1Charset
decodeFormat1 remainingGlyphs =
    Decode.loop ( remainingGlyphs, [] ) (calculateCount decodeRange1)
        |> Decode.map Array.fromList


decodeFormat2 : Int -> Decoder Format2Charset
decodeFormat2 remainingGlyphs =
    Decode.loop ( remainingGlyphs, [] ) (calculateCount decodeRange2)
        |> Decode.map Array.fromList


calculateCount rangeDecoder ( remainingGlyphs, accum ) =
    if remainingGlyphs <= 0 then
        Decode.succeed (Done (List.reverse accum))

    else
        decodeRange1
            |> Decode.andThen
                (\({ first, nLeft } as range) ->
                    Decode.succeed (Loop ( remainingGlyphs - (1 + nLeft), range :: accum ))
                )


decodeRange1 =
    Decode.map2 Range (Decode.unsignedInt16 BE) Decode.unsignedInt8


decodeRange2 =
    Decode.map2 Range (Decode.unsignedInt16 BE) (Decode.unsignedInt16 BE)



-- Get


{-| Returns the string ID for the supplied codepoint.
-}
get : Int -> Charset -> Int
get index charset =
    Debug.todo "charset get"
