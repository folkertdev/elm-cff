module Glyphs exposing (Glyphs, charstring, parse)

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Charset exposing (Charset)
import Charstring.Internal as Charstring exposing (Charstring, Operation, Segment, Subroutines)
import Dict exposing (Dict)
import Dict.Private exposing (Private)
import Dict.Top exposing (Top)
import Encoding exposing (Encodings)
import Index


type alias Glyphs =
    { top : Top
    , private : Private

    -- , subroutines : { global : Array (List Segment), local : Maybe (Array (List Segment)) }
    , encodings : Encodings
    , charset : Charset
    , charstrings : Array Charstring
    }


parse : Bytes -> Top -> Subroutines -> Maybe Glyphs
parse bytes top global =
    private top.private bytes
        |> Maybe.andThen
            (\( privateDict, localSubRoutines ) ->
                let
                    decodeCharstrings : Maybe (List Charstring)
                    decodeCharstrings =
                        case top.charstrings of
                            Nothing ->
                                Debug.log "no charstrings in this top dict" Nothing

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
                                Debug.log "decodeCharsetAndCharstrings failed" Nothing

                            Just charstrings ->
                                decodeWithOffset top.charset (Charset.decode { charset = top.charset, numberOfGlyphs = List.length charstrings }) bytes
                                    |> Maybe.map (\charset -> ( charstrings, charset ))

                    decodeEncoding =
                        decodeWithOffset top.encoding (Encoding.decode top.encoding) bytes

                    constructor encoding ( charstrings, charset ) =
                        { top = top
                        , private = privateDict

                        -- , subroutines = { global = global, local = localSubRoutines }
                        , encodings = encoding
                        , charset = charset
                        , charstrings = Array.fromList charstrings
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

                end =
                    offset + size
            in
            case decodeWithOffset start (Dict.Private.decode size) bytes of
                Nothing ->
                    let
                        _ =
                            Debug.log "private failed to decode, " ()
                    in
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
            Just ( Dict.Private.defaultPrivate, Nothing )


decodeWithOffset offset decoder bytes =
    Decode.decode (Decode.map2 (\_ k -> k) (Decode.bytes offset) decoder) bytes


charstring : Glyphs -> Int -> Maybe { width : Int, instructions : Charstring }
charstring glyphs index =
    case Array.get index glyphs.charstrings of
        Nothing ->
            Nothing

        Just cstring ->
            case cstring of
                (Charstring.Width w) :: rest ->
                    Just
                        { width = w + glyphs.private.nominal_width_x
                        , instructions = cstring
                        }

                _ ->
                    Just
                        { width = glyphs.private.default_width_x
                        , instructions = cstring
                        }
