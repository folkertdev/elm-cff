module Utils exposing (drop, exactly, keep, until, while)

import Bytes.Decode as Decode exposing (Decoder, Step(..))


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


keep : Decoder argument -> Decoder (argument -> result) -> Decoder result
keep argument function =
    Decode.map2 (<|) function argument


drop : Decoder drop -> Decoder keep -> Decoder keep
drop toDrop toKeep =
    Decode.map2 (\k _ -> k) toKeep toDrop


while : (a -> Bool) -> Decoder a -> Decoder ( a, List a )
while predicate =
    until (not << predicate)


until : (a -> Bool) -> Decoder a -> Decoder ( a, List a )
until predicate elementDecoder =
    Decode.loop [] (untilHelp predicate elementDecoder)


untilHelp predicate elementDecoder accum =
    elementDecoder
        |> Decode.andThen
            (\element ->
                if predicate element then
                    Decode.succeed (Done ( element, List.reverse accum ))

                else
                    Decode.succeed (Loop (element :: accum))
            )
