module Index exposing (OffsetSize, charstring, charstringWithOptions, name, offSize, string, subroutines, top)

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Bytes.Encode as Encode
import Charstring.Internal as Charstring exposing (Charstring, Operation, Segment, Subroutines, initialSubroutines)
import Dict.Top exposing (Top)


top : Decoder (List Top)
top =
    sizedIndex Dict.Top.decode


name : Decoder (List String)
name =
    sizedIndex Decode.string


string : Decoder (List String)
string =
    sizedIndex Decode.string
        |> Decode.map (\_ -> [])


localSubRoutines : Decoder (List (List Segment))
localSubRoutines =
    sizedIndex Charstring.decodeSegments


charstring : Decoder (List Charstring)
charstring =
    let
        context =
            { global = initialSubroutines
            , local = Nothing
            }
    in
    sizedIndex (\size -> Charstring.decode size context)


charstringWithOptions : { global : Subroutines, local : Maybe Subroutines } -> Decoder (List Charstring)
charstringWithOptions subs =
    sizedIndex (\size -> Charstring.decode size subs)


subroutines : Decoder Subroutines
subroutines =
    card16
        |> Decode.andThen
            (\count ->
                if count == 0 then
                    Decode.succeed initialSubroutines

                else
                    offSize
                        |> Decode.andThen
                            (\offsetSize ->
                                exactly (count + 1) (offset offsetSize)
                                    |> Decode.andThen
                                        (\offsets ->
                                            let
                                                deltas =
                                                    List.map2 (\small large -> large - small) offsets (List.drop 1 offsets)
                                            in
                                            mapM Decode.bytes deltas
                                                |> Decode.map Array.fromList
                                        )
                            )
            )



-- Helpers


sizedIndex : (Int -> Decode.Decoder a) -> Decode.Decoder (List a)
sizedIndex elementDecoder =
    card16
        |> Decode.andThen
            (\count ->
                if count == 0 then
                    Decode.succeed []

                else
                    offSize
                        |> Decode.andThen
                            (\offsetSize ->
                                exactly (count + 1) (offset offsetSize)
                                    |> Decode.andThen
                                        (\offsets ->
                                            let
                                                deltas =
                                                    List.map2 (\smaller larger -> larger - smaller) offsets (List.drop 1 offsets)
                                            in
                                            mapM elementDecoder deltas
                                        )
                            )
            )


sizedIndexDebug : (Int -> Decode.Decoder a) -> Decode.Decoder (List a)
sizedIndexDebug elementDecoder =
    card16
        |> Decode.andThen
            (\count ->
                if count == 0 then
                    Decode.succeed []

                else
                    offSize
                        |> Decode.andThen
                            (\offsetSize ->
                                exactly (count + 1) (offset offsetSize)
                                    |> Decode.andThen
                                        (\offsets ->
                                            let
                                                deltas =
                                                    List.map2 (\smaller larger -> larger - smaller) offsets (List.drop 1 offsets)
                                            in
                                            mapM
                                                (\( n, d ) ->
                                                    let
                                                        _ =
                                                            Debug.log "iteration" n
                                                    in
                                                    elementDecoder d
                                                )
                                                (List.indexedMap (\i k -> ( i, k )) deltas)
                                        )
                            )
            )


card8 : Decoder Int
card8 =
    Decode.unsignedInt8


card16 : Decoder Int
card16 =
    Decode.unsignedInt16 BE


type OffsetSize
    = Bytes1
    | Bytes2
    | Bytes3
    | Bytes4


offsetSizeInBytes s =
    case s of
        Bytes1 ->
            1

        Bytes2 ->
            2

        Bytes3 ->
            3

        Bytes4 ->
            4


offSize =
    Decode.unsignedInt8
        |> Decode.andThen
            (\size ->
                case size of
                    1 ->
                        Decode.succeed Bytes1

                    2 ->
                        Decode.succeed Bytes2

                    3 ->
                        Decode.succeed Bytes3

                    4 ->
                        Decode.succeed Bytes4

                    _ ->
                        let
                            _ =
                                Debug.log "invalid offset size " size
                        in
                        Decode.fail
            )


offset : OffsetSize -> Decoder Int
offset size =
    case size of
        Bytes1 ->
            Decode.unsignedInt8

        Bytes2 ->
            Decode.unsignedInt16 BE

        Bytes3 ->
            -- TODO check this logic
            Decode.succeed (\small large -> small + Bitwise.shiftLeftBy 16 large)
                |> keep (Decode.unsignedInt16 BE)
                |> keep Decode.unsignedInt8
                |> Decode.map (Debug.log "---------------> unchecked logic")

        Bytes4 ->
            Decode.unsignedInt32 BE


andMap first later =
    Decode.map2 (<|) later first


keep =
    andMap


skip drop preserve =
    Decode.map2 (\_ p -> p) drop preserve


mapM : (a -> Decoder b) -> List a -> Decoder (List b)
mapM f xs =
    Decode.loop ( [], xs ) (mapMHelp f)


mapMHelp : (a -> Decoder b) -> ( List b, List a ) -> Decoder (Step ( List b, List a ) (List b))
mapMHelp elementDecoder ( accum, remaining ) =
    case remaining of
        first :: rest ->
            Decode.map (\new -> Loop ( new :: accum, rest )) (elementDecoder first)

        [] ->
            Decode.succeed (Done (List.reverse accum))


exactlyDebug : Int -> Decoder a -> Decoder (List a)
exactlyDebug len decoder =
    let
        looper ( n, xs ) =
            let
                _ =
                    Debug.log "iteration" n
            in
            if n > len then
                Decode.succeed (Done (List.reverse xs))

            else
                Decode.map (\x -> Loop ( n + 1, x :: xs )) decoder
    in
    Decode.loop ( len, [] ) looper


exactly : Int -> Decoder a -> Decoder (List a)
exactly len decoder =
    Decode.loop ( len, [] ) (listStep decoder)


listStep : Decoder a -> ( Int, List a ) -> Decoder (Step ( Int, List a ) (List a))
listStep decoder ( n, xs ) =
    if n <= 0 then
        Decode.succeed (Done (List.reverse xs))

    else
        Decode.map (\x -> Loop ( n - 1, x :: xs )) decoder


last l =
    case l of
        [ x ] ->
            Just x

        x :: xs ->
            last xs

        [] ->
            Nothing
