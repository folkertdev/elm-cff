module Decode.Extra exposing (andMap, array, decodeWithOffset, dropLeft, exactly, fromMaybe, keep, mapM, sizedArray, unfold)

import Array exposing (Array)
import Bytes exposing (Bytes)
import Bytes.Decode as Decode exposing (Decoder, Step(..))


fromMaybe : Maybe a -> Decoder a
fromMaybe m =
    case m of
        Nothing ->
            Decode.fail

        Just v ->
            Decode.succeed v


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


andMap first later =
    Decode.map2 (<|) later first


exactly : Int -> Decoder a -> Decoder (List a)
exactly tableCount decoder =
    let
        helper ( n, xs ) =
            if n <= 0 then
                Decode.succeed (Done (List.reverse xs))

            else
                Decode.map (\x -> Loop ( n - 1, x :: xs )) decoder
    in
    Decode.loop ( tableCount, [] ) helper


array : Int -> Decoder a -> Decoder (Array a)
array count decoder =
    exactly count decoder
        |> Decode.map Array.fromList


sizedArray : Decoder Int -> Decoder a -> Decoder (Array a)
sizedArray sizeDecoder elementDecoder =
    sizeDecoder |> Decode.andThen (\size -> array size elementDecoder)


keep : Decoder argument -> Decoder (argument -> result) -> Decoder result
keep argument function =
    Decode.map2 (<|) function argument


drop : Decoder drop -> Decoder keep -> Decoder keep
drop toDrop toKeep =
    Decode.map2 (\k _ -> k) toKeep toDrop


decodeWithOffset offset decoder bytes =
    Decode.decode (Decode.map2 (\_ k -> k) (Decode.bytes offset) decoder) bytes


dropLeft : Int -> Bytes -> Maybe Bytes
dropLeft n bytes =
    let
        decoder =
            Decode.succeed (\_ k -> k)
                |> andMap (Decode.bytes n)
                |> andMap (Decode.bytes (Bytes.width bytes - n))
    in
    Decode.decode decoder bytes


split : Bytes -> List Int -> Decoder (List Bytes)
split buffer indices_ =
    let
        indices =
            0 :: (indices_ ++ [ Bytes.width buffer ])

        deltas =
            List.map2 (\smaller larger -> larger - smaller) indices (List.drop 1 indices)
    in
    mapM Decode.bytes deltas


unfold : (state -> Decoder (Step ( a, state ) ( a, state ))) -> state -> Decoder ( List a, state )
unfold step state =
    Decode.loop ( [], state ) (unfoldHelp step)


unfoldHelp step ( accum, state ) =
    step state
        |> Decode.map
            (\result ->
                case result of
                    Loop ( operation, newState ) ->
                        Loop ( operation :: accum, newState )

                    Done ( operation, newState ) ->
                        Done ( List.reverse (operation :: accum), newState )
            )
