module Dict.Internal exposing (Operator)

import Bitwise
import Bytes.Decode as Decode exposing (Decoder)
import Number


type Operator
    = Operator Int


decodeOperator : Decoder Operator
decodeOperator =
    Decode.unsignedInt8
        |> Decode.andThen
            (\operator ->
                if operator /= 12 then
                    Decode.succeed (Operator operator)

                else
                    Decode.unsignedInt8
                        |> Decode.andThen
                            (\operator2 ->
                                Decode.succeed (Operator <| Bitwise.shiftLeftBy 8 operator + operator2)
                            )
            )
