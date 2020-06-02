module Dict.Operator exposing
    ( Operator, Argument
    , decode
    , argumentToInt, argumentToFloat
    )

{-|

@docs Operator, Argument

@docs decode

@docs argumentToInt, argumentToFloat

-}

import Bitwise
import Bytes exposing (Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Decode.Extra


{-| Operators used in DICT structures

  - **opcode**: the code of the operator
  - **arguments**: arguments for the operator
  - **size**: the size in bytes of the whole operator

-}
type alias Operator =
    { opcode : Int, arguments : List Argument, size : Int }


decodeOperator : Decoder OpCode
decodeOperator =
    Decode.unsignedInt8
        |> Decode.andThen decodeOperatorHelp


decodeOperatorHelp : Int -> Decoder OpCode
decodeOperatorHelp operator =
    if operator /= 12 then
        Decode.succeed (OpCode False operator)

    else
        Decode.unsignedInt8
            |> Decode.map
                (\operator2 -> OpCode True <| Bitwise.shiftLeftBy 8 operator + operator2)


{-| Decode an `Operator`.
-}
decode : Decoder Operator
decode =
    Decode.loop ( 0, [] ) entryHelp


type OpCode
    = OpCode Bool Int


operatorSize : OpCode -> Int
operatorSize op =
    case op of
        OpCode twoBytes _ ->
            if twoBytes then
                2

            else
                1


entryHelp ( size, accum ) =
    Decode.unsignedInt8
        |> Decode.andThen numberHelp
        |> Decode.map
            (\first ->
                case first of
                    Err ((OpCode twoBytes opcode) as operator) ->
                        Done
                            { opcode = opcode
                            , arguments = List.reverse accum
                            , size = size + operatorSize operator
                            }

                    Ok ((Integer (Size numSize) _) as num) ->
                        Loop ( size + numSize, num :: accum )

                    Ok ((Real (Size numSize) _) as num) ->
                        Loop ( size + numSize, num :: accum )
            )


{-| -}
argumentToInt : Argument -> Int
argumentToInt num =
    case num of
        Integer _ i ->
            i

        Real _ f ->
            round f


{-| -}
argumentToFloat : Argument -> Float
argumentToFloat num =
    case num of
        Integer _ i ->
            Basics.toFloat i

        Real _ f ->
            f


{-| Dict Operator arguments
-}
type Argument
    = Integer Size Int
    | Real Size Float


type Size
    = Size Int


decodeArgument : Decoder Argument
decodeArgument =
    Decode.unsignedInt8
        |> Decode.andThen numberHelp
        |> Decode.andThen
            (\result ->
                case result of
                    Err _ ->
                        Decode.fail

                    Ok num ->
                        Decode.succeed num
            )


numberHelp : Int -> Decoder (Result OpCode Argument)
numberHelp first =
    if first >= 0x20 && first <= 0xF6 then
        Decode.succeed (Ok (Integer (Size 1) (first - 139)))

    else if first >= 0xF7 && first <= 0xFA then
        Decode.unsignedInt8
            |> Decode.map
                (\second ->
                    let
                        b0 =
                            (first - 247) * 256

                        b1 =
                            second
                    in
                    Ok (Integer (Size 2) (b0 + b1 + 108))
                )

    else if first >= 0xFB && first <= 0xFE then
        Decode.unsignedInt8
            |> Decode.map
                (\second ->
                    let
                        b0 =
                            (first - 251) * -256

                        b1 =
                            second
                    in
                    Ok (Integer (Size 2) (b0 + b1 - 108))
                )

    else
        case first of
            0x1C ->
                Decode.signedInt16 BE
                    |> Decode.map (Ok << Integer (Size 3))

            0x1D ->
                Decode.signedInt32 BE
                    |> Decode.map (Ok << Integer (Size 4))

            0x1E ->
                Decode.map (\( s, v ) -> Ok <| Real s v) real

            _ ->
                if first < 22 then
                    Decode.map Err (decodeOperatorHelp first)

                else
                    Decode.Extra.failWith ("Dict.Operator.numberHelp: opcode fell through " ++ String.fromInt first)


{-| Decode a floating-point number
-}
real : Decoder ( Size, Float )
real =
    Decode.loop ( "", Nothing, 0 ) parseRealHelp
        |> Decode.andThen
            (\( size, str ) ->
                case String.toFloat str of
                    Just v ->
                        Decode.succeed ( Size size, v )

                    Nothing ->
                        Decode.fail
            )


parseRealHelp ( buffer, high, size ) =
    case high of
        Nothing ->
            -- parse new byte
            Decode.unsignedInt8
                |> Decode.andThen
                    (\byte ->
                        case matchNibble (Bitwise.shiftRightBy 4 byte) of
                            Ok chars ->
                                Decode.succeed (Loop ( buffer ++ chars, Just (Bitwise.and byte 0x0F), size + 1 ))

                            Err True ->
                                Decode.succeed (Done ( size, buffer ))

                            Err False ->
                                Decode.fail
                    )

        Just nibble ->
            case matchNibble nibble of
                Ok chars ->
                    Decode.succeed (Loop ( buffer ++ chars, Nothing, size ))

                Err True ->
                    Decode.succeed (Done ( size, buffer ))

                Err False ->
                    Decode.fail


matchNibble : Int -> Result Bool String
matchNibble nibble =
    if nibble >= 0 && nibble <= 9 then
        (Char.toCode '0' + nibble)
            |> Char.fromCode
            |> String.fromChar
            |> Ok

    else
        case nibble of
            0x0A ->
                Ok "."

            0x0B ->
                Ok "e"

            0x0C ->
                Ok "e-"

            0x0E ->
                Ok "-"

            0x0F ->
                Err True

            _ ->
                Err False
