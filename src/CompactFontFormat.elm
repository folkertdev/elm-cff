module CompactFontFormat exposing
    ( CompactFont, parse
    , characterToName
    , CompactFontSet, pickFont
    , decodeCompactFont, decodeCompactFontSet
    )

{-| Decoding Compact Font Format (CFF) tables.

@docs CompactFont, parse


## Utility functions

@docs characterToName


## Lowlevel helpers

@docs CompactFontSet, pickFont
@docs decodeCompactFont, decodeCompactFontSet

-}

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Charset exposing (Charset)
import Charstring exposing (Charstring, Operation, Subroutines)
import Decode.CompactFontFormat exposing (GID(..), SID(..))
import Decode.Extra exposing (andMap)
import Dict exposing (Dict)
import Dict.Private exposing (Private)
import Dict.Top exposing (Top)
import Encoding
import Glyphs exposing (Glyphs)
import Index
import Strings exposing (Strings(..))


{-| -}
type alias Header =
    { version : { major : Int, minor : Int }
    , headerSize : Int
    , offsetSize : Decode.CompactFontFormat.OffsetSize
    }


decodeHeader : Decoder Header
decodeHeader =
    Decode.map3 Header
        Decode.CompactFontFormat.version
        Decode.unsignedInt8
        Decode.CompactFontFormat.offsetSize


{-| The CFF table

  - **header**: the CFF table header
  - **names**: names used in the table (for instance the font name)
  - **tops**: top-level dictionaries for all fonts in this table
  - **strings**: strings used in this table
  - **subroutines**: the global subroutines of this table

-}
type alias Cff =
    { header : Header
    , names : Array String
    , tops : Array Top
    , strings : Strings
    , subroutines : Subroutines
    }


{-| A single compact font

A single font is much easier to work with than a fontset, so it is the main abstraction.

  - **version**: font version
  - **name**: font name
  - **tops**: top-level dictionary
  - **strings**: glyph names
  - **subroutines**: global subroutines

-}
type alias CompactFont =
    { version : { major : Int, minor : Int }
    , name : String
    , topLevelDict : Top
    , strings : Strings
    , subroutines : Subroutines
    , glyphs : Glyphs
    }


{-| A compact font set

This is the raw decoded data. This means little safety and convenience, but maximum flexibility.

  - **version**: font set version
  - **name**: font names
  - **tops**: top-level dictionaries
  - **strings**: glyph names of all fonts
  - **subroutines**: global subroutines

-}
type alias CompactFontSet =
    { version : { major : Int, minor : Int }
    , names : Array String
    , tops : Array Top
    , strings : Array String
    , subroutines : Subroutines
    }


{-| Pick a font from a fontset
-}
pickFont : Bytes -> CompactFontSet -> Int -> Maybe CompactFont
pickFont buffer fontset index =
    Array.get index fontset.tops
        |> Maybe.andThen
            (\topLevelDict ->
                let
                    helper name fontGlyphs =
                        { version = fontset.version
                        , subroutines = fontset.subroutines
                        , name = name
                        , topLevelDict = topLevelDict
                        , strings = Strings fontset.strings
                        , glyphs = fontGlyphs
                        }

                    mGlyphs =
                        Glyphs.parse buffer topLevelDict fontset.subroutines
                in
                Maybe.map2 helper (Array.get index fontset.names) mGlyphs
            )


{-| Parse `Bytes` into a CFF font.

This function is defined in terms of `decodeCompactFontSet` and picks the first font in the font set.
In most cases, there is only one font in the font set.

-}
parse : Bytes -> Maybe CompactFont
parse bytes =
    Decode.decode (decodeCompactFont bytes) bytes


{-| The CFF table decoder
-}
decodeCompactFontSet : Decoder CompactFontSet
decodeCompactFontSet =
    Decode.succeed CompactFontSet
        |> andMap (Decode.map .version decodeHeader)
        |> andMap Index.name
        |> andMap Index.top
        |> andMap Index.string
        |> andMap Index.subroutines


{-| The CFF table decoder
-}
decodeCompactFont : Bytes -> Decoder CompactFont
decodeCompactFont buffer =
    decodeCompactFontSet
        |> Decode.andThen (\set -> Decode.Extra.fromMaybe (pickFont buffer set 0))



-- Utility functions


{-| Find the name of a character in this font
-}
characterToName : CompactFont -> (Char -> Int) -> Char -> String
characterToName font encoding character =
    let
        sid =
            Charset.glyphNameIdentifier (encoding character) font.glyphs.charset
    in
    Strings.get sid font.strings
