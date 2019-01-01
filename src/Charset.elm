module Charset exposing (Charset, decode)

{-| Charset data

@docs Charset, decode

-}

import Array exposing (Array)
import Bytes exposing (Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Decode.Extra


{-| The Charset
-}
type Charset
    = Format0 (Array Int)
    | Format1 (Array { first : Int, nLeft : Int })
    | Format2 (Array { first : Int, nLeft : Int })


range : Int -> Int -> { first : Int, nLeft : Int }
range first nLeft =
    { first = first, nLeft = nLeft }


decode : { charset : Int, numberOfGlyphs : Int } -> Decoder Charset
decode { charset, numberOfGlyphs } =
    Decode.unsignedInt8
        |> Decode.andThen
            (\format ->
                case format of
                    0 ->
                        decodeFormat0 numberOfGlyphs
                            |> Decode.map Format0

                    1 ->
                        decodeFormat1 numberOfGlyphs
                            |> Decode.map Format1

                    2 ->
                        decodeFormat2 numberOfGlyphs
                            |> Decode.map Format2

                    _ ->
                        Decode.fail
            )


decodeFormat0 : Int -> Decoder (Array Int)
decodeFormat0 nGlyphs =
    -- the .notdef glyph is not encoded, so - 1
    Decode.Extra.array (nGlyphs - 1) (Decode.unsignedInt16 BE)


decodeFormat1 : Int -> Decoder (Array { first : Int, nLeft : Int })
decodeFormat1 remainingGlyphs =
    Decode.loop ( remainingGlyphs, Array.empty ) (calculateCount decodeRange1)


decodeFormat2 : Int -> Decoder (Array { first : Int, nLeft : Int })
decodeFormat2 remainingGlyphs =
    Decode.loop ( remainingGlyphs, Array.empty ) (calculateCount decodeRange2)


calculateCount rangeDecoder ( remainingGlyphs, accum ) =
    if remainingGlyphs <= 0 then
        Decode.succeed (Done accum)

    else
        rangeDecoder
            |> Decode.map
                (\decodedRange ->
                    Loop ( remainingGlyphs - (1 + decodedRange.nLeft), Array.push decodedRange accum )
                )


decodeRange1 =
    Decode.map2 range (Decode.unsignedInt16 BE) Decode.unsignedInt8


decodeRange2 =
    Decode.map2 range (Decode.unsignedInt16 BE) (Decode.unsignedInt16 BE)



-- Get


{-| Returns the string ID for the supplied codepoint.
-}
get : Int -> Charset -> Int
get index charset =
    Debug.todo "charset get"
