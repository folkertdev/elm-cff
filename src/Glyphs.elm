module Glyphs exposing
    ( Glyphs
    , parse
    , charstring
    )

{-| The set of glyphs in the font

@docs Glyphs
@docs parse
@docs charstring

-}

import Array exposing (Array)
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Step(..))
import Charset exposing (Charset)
import Charstring exposing (Charstring, Subroutines)
import Decode.CompactFontFormat exposing (GID(..), SID(..))
import Dict.Private exposing (Private)
import Dict.Top exposing (Top)
import Encoding exposing (Encoding)
import Index


{-| Set of glyphs in the font
-}
type alias Glyphs =
    { top : Top
    , private : Private
    , encoding : Encoding
    , charset : Charset
    , charstrings : Array Charstring
    }


{-| Parse a `Glyphs`
-}
parse : Bytes -> Top -> Subroutines -> Maybe Glyphs
parse bytes top global =
    private top.private bytes
        |> Maybe.andThen
            (\( privateDict, localSubRoutines ) ->
                let
                    decodeCharstrings : Maybe (Array Charstring)
                    decodeCharstrings =
                        case top.charstrings of
                            Nothing ->
                                -- ERROR no charstrings in this top dict
                                Nothing

                            Just internalOffset ->
                                let
                                    context =
                                        { global = global
                                        , local = localSubRoutines
                                        }
                                in
                                decodeWithOffset internalOffset (Index.charstringWithOptions context) bytes

                    decodeCharsetAndCharstrings =
                        case decodeCharstrings of
                            Nothing ->
                                -- ERROR decodeCharsetAndCharstrings failed
                                Nothing

                            Just charstrings ->
                                decodeWithOffset top.charset (Charset.decode { offset = top.charset, numberOfGlyphs = Array.length charstrings }) bytes
                                    --decodeWithOffset top.charset (Charset.alternative (Array.length charstrings)) bytes
                                    |> Maybe.map (\currentCharset -> ( charstrings, currentCharset ))

                    decodeEncoding =
                        decodeWithOffset top.encoding (Encoding.decode { offset = top.encoding }) bytes

                    -- decodeWithOffset top.encoding (Encoding.alternative top.encoding) bytes
                    constructor encodings ( charstrings, currentCharset ) =
                        { top = top
                        , private = privateDict
                        , encoding = encodings
                        , charset = currentCharset
                        , charstrings = charstrings
                        }
                in
                Maybe.map2 constructor
                    decodeEncoding
                    decodeCharsetAndCharstrings
            )


private : Maybe { size : Int, offset : Int } -> Bytes -> Maybe ( Private, Maybe Subroutines )
private arguments bytes =
    case arguments of
        Just { size, offset } ->
            let
                start =
                    offset
            in
            case decodeWithOffset start (Dict.Private.decode size) bytes of
                Nothing ->
                    -- ERROR private failed to decode,
                    Nothing

                Just privateDict ->
                    case privateDict.subroutines of
                        Nothing ->
                            Just ( privateDict, Nothing )

                        Just subroutineOffset ->
                            case decodeWithOffset (start + subroutineOffset) Index.subroutines bytes of
                                Nothing ->
                                    Just ( privateDict, Nothing )

                                Just localSubroutines ->
                                    Just ( privateDict, Just localSubroutines )

        Nothing ->
            Just ( Dict.Private.default, Nothing )


decodeWithOffset : Int -> Decode.Decoder a -> Bytes -> Maybe a
decodeWithOffset offset decoder bytes =
    Decode.decode (Decode.map2 (\_ k -> k) (Decode.bytes offset) decoder) bytes


{-| Get the charstring of a particular glyph
-}
charstring : Int -> Glyphs -> Charstring
charstring index glyphs =
    case Array.get index glyphs.charstrings of
        Nothing ->
            []

        Just cstring ->
            cstring
