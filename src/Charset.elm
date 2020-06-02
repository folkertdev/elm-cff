module Charset exposing
    ( Charset, decode
    , glyphNameIdentifier
    )

{-| The Charset maps glyph identifiers to glyph names

@docs Charset, decode
@docs glyphNameIdentifier

-}

import Array exposing (Array)
import Bytes exposing (Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Decode.CompactFontFormat exposing (GID(..), SID(..))
import Decode.Extra


{-| The Charset maps glyph identifiers to string identifiers.

The string identifiers can then be used as indices into the string INDEX to get the actual name of a glyph.

-}
type Charset
    = Format0 (Array Int)
    | Format1 (Array { first : SID, nLeft : Int })
    | Format2 (Array { first : SID, nLeft : Int })


range : SID -> Int -> { first : SID, nLeft : Int }
range first nLeft =
    { first = first, nLeft = nLeft }


{-| Decode a `Charset`. The `offset` is (a)bused to indicate use of a predefined charset

  - **offset = 0**: the ISOAdobe charset will be used
  - **offset = 1**: the Expert charset will be used
  - **offset = 2**: the ExpertSubset charset will be used
  - **otherwise** the charset has to be decoded

When a predefined charset is used the charset is not encoded.

-}
decode : { offset : Int, numberOfGlyphs : Int } -> Decoder Charset
decode { offset, numberOfGlyphs } =
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
                        Decode.Extra.failWith ("charset format unsupported: " ++ String.fromInt format)
            )


alternative : Int -> Decoder (Array SID)
alternative numberOfGlyphs =
    Decode.unsignedInt8
        |> Decode.andThen
            (\format ->
                case format of
                    0 ->
                        Decode.Extra.array numberOfGlyphs Decode.CompactFontFormat.sid

                    1 ->
                        let
                            looper charset =
                                if Array.length charset <= numberOfGlyphs then
                                    Decode.CompactFontFormat.sid
                                        |> Decode.andThen
                                            (\(SID id) ->
                                                Decode.unsignedInt8
                                                    |> Decode.andThen
                                                        (\count ->
                                                            Decode.succeed (Loop (go id 0 count charset))
                                                        )
                                            )

                                else
                                    Decode.succeed (Done charset)

                            go id i count charset =
                                if i <= count then
                                    go (id + 1) (i + 1) count (Array.push (SID id) charset)

                                else
                                    charset
                        in
                        Decode.loop Array.empty looper

                    2 ->
                        let
                            looper charset =
                                if Array.length charset <= numberOfGlyphs then
                                    Decode.CompactFontFormat.sid
                                        |> Decode.andThen
                                            (\(SID id) ->
                                                Decode.unsignedInt16 BE
                                                    |> Decode.andThen
                                                        (\count ->
                                                            Decode.succeed (Loop (go id 0 count charset))
                                                        )
                                            )

                                else
                                    Decode.succeed (Done charset)

                            go id i count charset =
                                if i <= count then
                                    go (id + 1) (i + 1) count (Array.push (SID id) charset)

                                else
                                    charset
                        in
                        Decode.loop Array.empty looper

                    _ ->
                        Decode.Extra.failWith "unsupported charset format"
            )


decodeFormat0 : Int -> Decoder (Array Int)
decodeFormat0 nGlyphs =
    -- the .notdef glyph is not encoded, so - 1
    Decode.Extra.array (nGlyphs - 1) (Decode.unsignedInt16 BE)


decodeFormat1 : Int -> Decoder (Array { first : SID, nLeft : Int })
decodeFormat1 remainingGlyphs =
    Decode.loop ( remainingGlyphs, Array.empty ) (calculateCount decodeRange1)


decodeFormat2 : Int -> Decoder (Array { first : SID, nLeft : Int })
decodeFormat2 remainingGlyphs =
    Decode.loop ( remainingGlyphs, Array.empty ) (calculateCount decodeRange2)


{-| Format1 and Format2 store the glyphs as ranges. This means one element in the charset array can cover many glyphs.
Since we only know the number of glyphs (and not the length of the charstring array) we have to do some bookkeeping:
for every range we decode, we must subtract the total size of that range from the number of remaining glyphs
-}
calculateCount :
    Decoder { first : SID, nLeft : Int }
    -> ( Int, Array { first : SID, nLeft : Int } )
    -> Decoder (Step ( Int, Array { first : SID, nLeft : Int } ) (Array { first : SID, nLeft : Int }))
calculateCount rangeDecoder ( remainingGlyphs, accum ) =
    if remainingGlyphs <= 0 then
        Decode.succeed (Done accum)

    else
        rangeDecoder
            |> Decode.map
                (\decodedRange ->
                    Loop ( remainingGlyphs - (1 + decodedRange.nLeft), Array.push decodedRange accum )
                )


{-| Format 1 range has u8 as nLeft
-}
decodeRange1 : Decoder { first : SID, nLeft : Int }
decodeRange1 =
    Decode.map2 range Decode.CompactFontFormat.sid Decode.unsignedInt8


{-| Format 2 range has u16 as nLeft
-}
decodeRange2 : Decoder { first : SID, nLeft : Int }
decodeRange2 =
    Decode.map2 range Decode.CompactFontFormat.sid (Decode.unsignedInt16 BE)



-- Get


unSID : SID -> Int
unSID (SID n) =
    n


{-| Find the string identifier for a glyph identifier.
-}
glyphNameIdentifier : Int -> Charset -> Int
glyphNameIdentifier index charset =
    case charset of
        Format0 array ->
            if index == 0 then
                0

            else
                Array.get (index - 1) array
                    |> Maybe.withDefault 0

        Format1 array ->
            getRange index array
                |> unSID

        Format2 array ->
            getRange index array
                |> unSID


getRange : Int -> Array { first : SID, nLeft : Int } -> SID
getRange index array =
    let
        folder currentRange accum =
            case accum of
                Ok done ->
                    Ok done

                Err currentIndex ->
                    if currentIndex < currentRange.nLeft + 1 then
                        let
                            (SID first) =
                                currentRange.first
                        in
                        Ok (SID (first + currentIndex))

                    else
                        Err (currentIndex - (currentRange.nLeft + 1))
    in
    if index == 0 then
        SID 0

    else
        Array.foldl folder (Err (index - 1)) array |> Result.withDefault (SID 0)
