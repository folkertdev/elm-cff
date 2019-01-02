module Index exposing (charstring, charstringWithOptions, name, string, subroutines, top)

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Charstring.Internal as Charstring exposing (Charstring, Subroutines)
import Decode.CompactFontFormat
import Decode.Extra exposing (andMap)
import Dict.Top exposing (Top)


top : Decoder (Array Top)
top =
    sizedIndex Dict.Top.decode
        |> Decode.map Array.fromList


name : Decoder (List String)
name =
    sizedIndex Decode.string


string : Decoder (List String)
string =
    sizedIndex Decode.string


charstring : Decoder (List Charstring)
charstring =
    let
        context =
            { global = Array.empty
            , local = Nothing
            }
    in
    sizedIndex (\size -> Charstring.decode context)


charstringWithOptions : { global : Subroutines, local : Maybe Subroutines } -> Decoder (List Charstring)
charstringWithOptions subs =
    sizedIndex (\size -> Charstring.decode subs)


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
