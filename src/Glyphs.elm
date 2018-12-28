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
import Index exposing (OffsetSize)


type alias Glyphs =
    { top : Top
    , private : Private

    -- , subroutines : { global : Array (List Segment), local : Maybe (Array (List Segment)) }
    , encodings : Encodings
    , charset : Charset
    , charstrings : Array Charstring
    }


parse : Int -> Bytes -> Top -> Subroutines -> Maybe Glyphs
parse offset bytes top global =
    parse2 offset bytes top global



{-
   let
       decodeCharstrings : Maybe (List Charstring)
       decodeCharstrings =
           case top.charstrings of
               Nothing ->
                   Nothing

               Just internalOffset ->
                   decodeWithOffset (offset + internalOffset) Index.charstring bytes

       groups =
           case top.cid of
               Nothing ->
                   Nothing

               Just cid ->
                   Debug.todo "select and tops"

       decodeEncoding =
           decodeWithOffset top.encoding (Encoding.decode top.encoding) bytes

       decodeCharsetAndCharstrings =
           decodeCharstrings
               |> Maybe.andThen
                   (\charstrings ->
                       decodeWithOffset top.charset (Charset.decode top.charset (List.length charstrings)) bytes
                           |> Maybe.map (\charset -> ( charstrings, charset ))
                   )

       constructor ( privateDict, localSubRoutines ) encoding ( charstrings, charset ) =
           { top = top
           , private = privateDict
           , subroutines = { global = global, local = localSubRoutines }
           , encodings = encoding
           , charset = charset
           , charstrings = charstrings
           }
   in
   Maybe.map3 constructor
       (private top.private bytes)
       decodeEncoding
       decodeCharsetAndCharstrings
-}


parse2 : Int -> Bytes -> Top -> Subroutines -> Maybe Glyphs
parse2 offset bytes top global =
    private offset top.private bytes
        |> Maybe.andThen
            (\( privateDict, localSubRoutines ) ->
                let
                    _ =
                        Debug.log "private decoded" ( Array.length global, localSubRoutines )

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
                                decodeWithOffset (offset + internalOffset) (Index.charstringWithOptions context) bytes

                    decodeCharsetAndCharstrings =
                        case decodeCharstrings of
                            Nothing ->
                                Debug.log "decodeCharsetAndCharstrings failed" Nothing

                            Just charstrings ->
                                decodeWithOffset (offset + top.charset) (Charset.decode top.charset (List.length charstrings)) bytes
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


private : Int -> Maybe { size : Int, offset : Int } -> Bytes -> Maybe ( Private, Maybe Subroutines )
private globalOffset arguments bytes =
    case arguments of
        Just { size, offset } ->
            let
                start =
                    globalOffset + offset

                end =
                    globalOffset + offset + size
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
                                    let
                                        _ =
                                            Debug.log "private decoded, no subs " ()
                                    in
                                    Just ( privateDict, Nothing )

                                Just localSubroutines ->
                                    let
                                        _ =
                                            Debug.log "private decoded, with subs" (Array.length localSubroutines)
                                    in
                                    Just ( privateDict, Just localSubroutines )

        Nothing ->
            Just ( Dict.Private.defaultPrivate, Nothing )


decodeWithOffset offset decoder bytes =
    Decode.decode (Decode.map2 (\_ k -> k) (Decode.string offset) decoder) bytes


charstring : Int -> Glyphs -> Maybe Charstring
charstring index glyphs =
    Array.get index glyphs.charstrings
