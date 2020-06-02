module Index exposing
    ( top, subroutines, name, string
    , charstring, charstringWithOptions
    )

{-| Decoders for CFF Index structures

@docs top, subroutines, name, string
@docs charstring, charstringWithOptions

-}

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Charstring exposing (Charstring, Subroutines)
import Decode.CompactFontFormat
import Decode.Extra exposing (andMap)
import Dict.Top exposing (Top)


{-| Decode the top-level dicts for all fonts in the fontset
-}
top : Decoder (Array Top)
top =
    sizedIndex Dict.Top.decode
        |> Decode.map Array.fromList


{-| Decode the name INDEX.

The name index contains the names of the fonts in the fontset

-}
name : Decoder (Array String)
name =
    sizedIndex Decode.string
        |> Decode.map Array.fromList


{-| Decode the string INDEX

All the strings, with the exception of the FontName and CIDFontName strings (which appear in the Name INDEX), used by different fonts in the fontset

-}
string : Decoder (Array String)
string =
    sizedIndex Decode.string
        |> Decode.map Array.fromList


{-| Decode a charstring INDEX
-}
charstring : Decoder (Array Charstring)
charstring =
    charstringWithOptions { global = Array.empty, local = Nothing }


{-| Decode a charstring INDEX with custom global and local subroutines
-}
charstringWithOptions : { global : Subroutines, local : Maybe Subroutines } -> Decoder (Array Charstring)
charstringWithOptions subs =
    sizedIndex (\size -> Charstring.decode subs)
        |> Decode.map Array.fromList


{-| Decode a subroutine INDEX
-}
subroutines : Decoder Subroutines
subroutines =
    sizedIndex Decode.bytes
        |> Decode.map Array.fromList



-- Helpers


sizedIndex : (Int -> Decode.Decoder a) -> Decode.Decoder (List a)
sizedIndex elementDecoder =
    Decode.CompactFontFormat.card16
        |> Decode.andThen
            (\count ->
                if count == 0 then
                    Decode.succeed []

                else
                    Decode.CompactFontFormat.offsetSize
                        |> Decode.andThen
                            (\offsetSize ->
                                Decode.Extra.exactly (count + 1) (Decode.CompactFontFormat.offset offsetSize)
                                    |> Decode.andThen
                                        (\offsets ->
                                            let
                                                deltas =
                                                    List.map2 (\smaller larger -> larger - smaller) offsets (List.drop 1 offsets)
                                            in
                                            Decode.Extra.mapM elementDecoder deltas
                                        )
                            )
            )
