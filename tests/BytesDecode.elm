module BytesDecode exposing (suite)

import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Charstring.Internal as Charstring exposing (Operation(..), Point)
import Charstring.Number exposing (Number(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Utils


suite =
    describe "charstring"
        [ test "exactly returns the correct amounts" <|
            \_ ->
                let
                    bytes =
                        Encode.sequence (List.map Encode.unsignedInt8 expected)
                            |> Encode.encode

                    expected =
                        [ 1, 2, 3, 4, 5 ]
                in
                Decode.decode (Utils.exactly 5 Decode.unsignedInt8) bytes
                    |> Expect.equal (Just expected)
        ]
