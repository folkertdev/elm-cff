module CompactFontFormat exposing
    ( parse, decode, glyphs
    , Cff, Header
    )

{-| Module docs

@docs parse, decode, glyphs


## Types

@docs Cff, Header

-}

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Charset exposing (Charset)
import Charstring.Internal as Charstring exposing (Charstring, Operation, Segment, Subroutines)
import Decode.CompactFontFormat
import Decode.Extra exposing (andMap)
import Dict exposing (Dict)
import Dict.Private exposing (Private)
import Dict.Top exposing (Top)
import Encoding exposing (Encodings)
import Glyphs exposing (Glyphs)
import Index


{-| CFF table header
-}
type alias Header =
    { major : Int
    , minor : Int
    , headerSize : Int
    , offsetSize : Decode.CompactFontFormat.OffsetSize
    }


decodeHeader : Decoder Header
decodeHeader =
    Decode.map4 Header
        Decode.unsignedInt8
        Decode.unsignedInt8
        Decode.unsignedInt8
        Decode.CompactFontFormat.offsetSize


{-| The CFF table
-}
type alias Cff =
    { header : Header
    , names : List String
    , tops : Array Top
    , strings : List String
    , subroutines : Subroutines
    }


{-| Parse `Bytes` into a CFF table
-}
parse : Bytes -> Maybe Cff
parse bytes =
    Decode.decode decode bytes


{-| The CFF table decoder
-}
decode : Decoder Cff
decode =
    Decode.succeed Cff
        |> andMap decodeHeader
        |> andMap Index.name
        |> andMap Index.top
        |> andMap Index.string
        |> andMap Index.subroutines


{-| Get the glyph data for the font at the given index.

In most cff tables, there is only one font (index 0)

Assumes the buffer starts at the start of the CFF table

-}
glyphs : Bytes -> Cff -> Int -> Maybe Glyphs
glyphs bytes cff index =
    Array.get index cff.tops
        |> Maybe.andThen (\top -> Glyphs.parse bytes top cff.subroutines)
