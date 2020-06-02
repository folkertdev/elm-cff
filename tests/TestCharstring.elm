module TestCharstring exposing (suite)

import Array
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Charstring exposing (Operation(..), Point)
import Charstring.Number exposing (Number(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite =
    describe "charstring"
        [ subrcall
        , gsubrcall
        , hstem
        , vstem
        , rmoveto
        , vmoveto
        , hmoveto
        , rlineto
        , hlineto
        , vlineto
        , rrcurveto
        , vvcurveto
        , hhcurveto
        , vhcurveto
        , hvcurveto
        , hintmask
        , cntrmask
        , rcurveline
        , rlinecurve
        , number
        ]


emptyBytes =
    Encode.encode (Encode.sequence [])


encode args operators =
    Encode.encode
        (Encode.sequence
            [ Encode.sequence (List.map Charstring.Number.encode args)
            , Encode.sequence (List.map Encode.unsignedInt8 operators)
            ]
        )


testCharstring name args operators result =
    test name <|
        \_ ->
            let
                bytes =
                    encode args operators

                decoder =
                    Charstring.decode { global = Array.empty, local = Nothing }
            in
            Decode.decode decoder bytes
                |> Expect.equal (Just result)


hstem =
    describe "hstem"
        [ testCharstring "hstem 1" [ 121, -21 ] [ 1, 14 ] [ HStem 121 100 ]
        , testCharstring "hstem 2" [ 121, -21, 400, -20 ] [ 1, 14 ] [ HStem 121 100, HStem 500 480 ]
        , testCharstring "hstem 3" [ 32, 121, -21 ] [ 1, 14 ] [ Width 32, HStem 121 100 ]
        , testCharstring "hstem 4" [ 32, 121, -21, 400, -20 ] [ 1, 14 ] [ Width 32, HStem 121 100, HStem 500 480 ]
        ]


vstem =
    describe "vstem"
        [ testCharstring "vstem 1" [ 121, -21 ] [ 3, 14 ] [ VStem 121 100 ]
        , testCharstring "vstem 2" [ 121, -21, 400, -20 ] [ 3, 14 ] [ VStem 121 100, VStem 500 480 ]
        , testCharstring "vstem 3" [ 32, 121, -21 ] [ 3, 14 ] [ Width 32, VStem 121 100 ]
        , testCharstring "vstem 4" [ 32, 121, -21, 400, -20 ] [ 3, 14 ] [ Width 32, VStem 121 100, VStem 500 480 ]
        ]


vmoveto =
    describe "vmoveto"
        [ testCharstring "vmoveto 1" [ 32 ] [ 4, 14 ] [ MoveTo { x = 0, y = 32 } ]
        , testCharstring "vmoveto 2" [ 32, 64 ] [ 4, 14 ] [ Width 32, MoveTo { x = 0, y = 64 } ]
        ]


rlineto =
    describe "rlineto"
        [ testCharstring "rlineto 1" [ 32, 64 ] [ 5, 14 ] [ LineTo { x = 32, y = 64 } ]
        , testCharstring "rlineto 2" [ 32, 64, 96, 128 ] [ 5, 14 ] [ LineTo { x = 32, y = 64 }, LineTo { x = 128, y = 192 } ]
        ]


hlineto =
    describe "hlineto"
        [ testCharstring "hlineto 1" [ 32 ] [ 6, 14 ] [ LineTo (Point 32 0) ]
        , testCharstring "hlineto 2" [ 32, 64 ] [ 6, 14 ] [ LineTo (Point 32 0), LineTo (Point 32 64) ]
        , testCharstring "hlineto 3" [ 32, 64, 96 ] [ 6, 14 ] [ LineTo (Point 32 0), LineTo (Point 32 64), LineTo (Point 128 64) ]
        , testCharstring "hlineto 4" [ 32, 64, 96, 128 ] [ 6, 14 ] [ LineTo (Point 32 0), LineTo (Point 32 64), LineTo (Point 128 64), LineTo (Point 128 192) ]
        ]


point ( x, y ) =
    Point x y


vlineto =
    describe "vlineto"
        [ testCharstring "vlineto 1" [ 32 ] [ 7, 14 ] [ LineTo (point ( 0, 32 )) ]
        , testCharstring "vlineto 2" [ 32, 64 ] [ 7, 14 ] [ LineTo (point ( 0, 32 )), LineTo (point ( 64, 32 )) ]
        , testCharstring "vlineto 3" [ 32, 64, 96 ] [ 7, 14 ] [ LineTo (point ( 0, 32 )), LineTo (point ( 64, 32 )), LineTo (point ( 64, 128 )) ]
        , testCharstring "vlineto 4" [ 32, 64, 96, 128 ] [ 7, 14 ] [ LineTo (point ( 0, 32 )), LineTo (point ( 64, 32 )), LineTo (point ( 64, 128 )), LineTo (point ( 192, 128 )) ]
        ]


rrcurveto =
    describe "rrcurveto"
        [ testCharstring "rrcurveto 1" [ 32, 64, 96, 128, 160, 192 ] [ 8, 14 ] [ CurveTo (point ( 32, 64 )) (point ( 128, 192 )) (point ( 288, 384 )) ]
        , testCharstring "rrcurveto 2"
            [ 32, 64, 96, 128, 160, 192, -32, -64, -96, -128, -160, -192 ]
            [ 8, 14 ]
            [ CurveTo (point ( 32, 64 )) (point ( 128, 192 )) (point ( 288, 384 ))
            , CurveTo (point ( 256, 320 )) (point ( 160, 192 )) (point ( 0, 0 ))
            ]
        ]


subrcall =
    describe "local subroutine call"
        [ test "subr 1" <|
            \_ ->
                let
                    subroutine =
                        Encode.encode
                            (Encode.sequence
                                [ Encode.sequence (List.map Charstring.Number.encode [ 128, 154 ])
                                , Encode.unsignedInt8 11
                                ]
                            )

                    bytes =
                        Encode.encode
                            (Encode.sequence
                                [ Encode.sequence (List.map Charstring.Number.encode [ 0 ])
                                , Encode.unsignedInt8 10
                                , Encode.unsignedInt8 1
                                , Encode.unsignedInt8 14
                                ]
                            )

                    expected =
                        [ HStem 128 282
                        ]

                    locals =
                        Array.repeat 200 emptyBytes |> Array.set 107 subroutine

                    decoder =
                        Charstring.decode { global = Array.empty, local = Just locals }
                in
                Decode.decode decoder bytes
                    |> Expect.equal (Just expected)
        , test "subr 2" <|
            \_ ->
                let
                    width =
                        5

                    offset =
                        1

                    subroutine =
                        Encode.encode
                            (Encode.sequence
                                [ Encode.sequence (List.map Charstring.Number.encode [ 400, 200 ])
                                , Encode.unsignedInt8 11
                                ]
                            )

                    bytes =
                        Encode.encode
                            (Encode.sequence
                                [ Encode.sequence (List.map Charstring.Number.encode [ 400, 200, 0 ])
                                , Encode.unsignedInt8 10
                                , Encode.unsignedInt8 1
                                , Encode.unsignedInt8 14
                                ]
                            )

                    expected =
                        [ HStem 400 600, HStem 1000 1200 ]

                    locals =
                        Array.repeat 200 emptyBytes |> Array.set 107 subroutine

                    decoder =
                        Charstring.decode { global = Array.empty, local = Just locals }
                in
                Decode.decode decoder bytes
                    |> Expect.equal (Just expected)
        ]


gsubrcall =
    describe "global subroutine call"
        [ test "gsubr 1" <|
            \_ ->
                let
                    subroutine =
                        Encode.encode
                            (Encode.sequence
                                [ Encode.sequence (List.map Charstring.Number.encode [ 128, 154 ])
                                , Encode.unsignedInt8 11
                                ]
                            )

                    bytes =
                        Encode.encode
                            (Encode.sequence
                                [ Encode.sequence (List.map Charstring.Number.encode [ 0 ])
                                , Encode.unsignedInt8 29
                                , Encode.unsignedInt8 1
                                , Encode.unsignedInt8 14
                                ]
                            )

                    expected =
                        [ HStem 128 282
                        ]

                    globals =
                        Array.repeat 200 emptyBytes |> Array.set 107 subroutine

                    decoder =
                        Charstring.decode { global = globals, local = Nothing }
                in
                Decode.decode decoder bytes
                    |> Expect.equal (Just expected)
        , test "gsubr 2" <|
            \_ ->
                let
                    width =
                        5

                    offset =
                        1

                    subroutine =
                        Encode.encode
                            (Encode.sequence
                                [ Encode.sequence (List.map Charstring.Number.encode [ 400, 200 ])
                                , Encode.unsignedInt8 11
                                ]
                            )

                    bytes =
                        Encode.encode
                            (Encode.sequence
                                [ Encode.sequence (List.map Charstring.Number.encode [ 400, 200, 0 ])
                                , Encode.unsignedInt8 29
                                , Encode.unsignedInt8 1
                                , Encode.unsignedInt8 14
                                ]
                            )

                    expected =
                        [ HStem 400 600, HStem 1000 1200 ]

                    globals =
                        Array.repeat 200 emptyBytes |> Array.set 107 subroutine

                    decoder =
                        Charstring.decode { global = globals, local = Nothing }
                in
                Decode.decode decoder bytes
                    |> Expect.equal (Just expected)
        ]


hintmask =
    describe "hintmask"
        [ test "hintmask 1" <|
            \_ ->
                let
                    bytes =
                        Encode.encode
                            (Encode.sequence
                                [ Encode.sequence (List.map Charstring.Number.encode [ 121, -21, 400, -20 ])
                                , Encode.unsignedInt8 1
                                , Encode.sequence (List.map Charstring.Number.encode [ 121, -21, 400, -20 ])
                                , Encode.unsignedInt8 19

                                -- v the actual mask
                                , Encode.unsignedInt8 99
                                , Encode.unsignedInt8 14
                                ]
                            )

                    expected =
                        [ HStem 121 100
                        , HStem 500 480
                        , VStem 121 100
                        , VStem 500 480
                        , HintMask [ 99 ]
                        ]

                    decoder =
                        Charstring.decode { global = Array.empty, local = Nothing }
                in
                Decode.decode decoder bytes
                    |> Expect.equal (Just expected)
        ]


cntrmask =
    describe "cntrmask"
        [ test "cntrmask 1" <|
            \_ ->
                let
                    bytes =
                        Encode.encode
                            (Encode.sequence
                                [ Encode.sequence (List.map Charstring.Number.encode [ 121, -21, 400, -20 ])
                                , Encode.unsignedInt8 1
                                , Encode.sequence (List.map Charstring.Number.encode [ 121, -21, 400, -20 ])
                                , Encode.unsignedInt8 20
                                , Encode.unsignedInt8 10
                                , Encode.unsignedInt8 14
                                ]
                            )

                    expected =
                        [ HStem 121 100
                        , HStem 500 480
                        , VStem 121 100
                        , VStem 500 480
                        , CounterMask [ 10 ]
                        ]

                    decoder =
                        Charstring.decode { global = Array.empty, local = Nothing }
                in
                Decode.decode decoder bytes
                    |> Expect.equal (Just expected)
        ]


rmoveto =
    describe "rmoveto"
        [ testCharstring "rmoveto 1" [ 32, 64 ] [ 21, 14 ] [ MoveTo { x = 32, y = 64 } ]
        , testCharstring "rmoveto 2" [ 32, 64, 96 ] [ 21, 14 ] [ Width 32, MoveTo { x = 64, y = 96 } ]
        ]


hmoveto =
    describe "hmoveto"
        [ testCharstring "hmoveto 1" [ 32 ] [ 22, 14 ] [ MoveTo { x = 32, y = 0 } ]
        , testCharstring "hmoveto 2" [ 32, 64 ] [ 22, 14 ] [ Width 32, MoveTo { x = 64, y = 0 } ]
        ]


rcurveline =
    describe "rcurveline"
        [ testCharstring "rcurveline 1"
            [ 32, 64, 96, 128, 160, 192, -32, -64 ]
            [ 24, 14 ]
            [ CurveTo (Point 32 64) (Point 128 192) (Point 288 384)
            , LineTo (Point 256 320)
            ]
        , testCharstring "rcurveline 2"
            [ 32, 64, 96, 128, 160, 192, -32, -64, -96, -128, -160, -192, 32, 64 ]
            [ 24, 14 ]
            [ CurveTo (Point 32 64) (Point 128 192) (Point 288 384)
            , CurveTo (Point 256 320) (Point 160 192) (Point 0 0)
            , LineTo (Point 32 64)
            ]
        ]


rlinecurve =
    describe "rlinecurve"
        [ testCharstring "rlinecurve 1"
            [ 32, 64, 96, 128, 160, 192, -32, -64 ]
            [ 25, 14 ]
            [ LineTo (Point 32 64)
            , CurveTo (Point 128 192) (Point 288 384) (Point 256 320)
            ]
        , testCharstring "rlinecurve 2"
            [ 32, 64, 96, 128, 160, 192, -32, -64, -128, -160 ]
            [ 25, 14 ]
            [ LineTo (Point 32 64)
            , LineTo (Point 128 192)
            , CurveTo (Point 288 384) (Point 256 320) (Point 128 160)
            ]
        ]


vvcurveto =
    describe "vvcurveto"
        [ testCharstring "vvcurveto 1"
            [ 32, 64, 96, 128 ]
            [ 26, 14 ]
            [ CurveTo (Point 0 32) (Point 64 128) (Point 64 256)
            ]
        , testCharstring "vvcurveto 2"
            [ 32, 64, 96, 128, 160 ]
            [ 26, 14 ]
            [ CurveTo (Point 32 64) (Point 128 192) (Point 128 352)
            ]
        , testCharstring "vvcurveto 3"
            [ 32, 64, 96, 128, -128, -96, -64, -32 ]
            [ 26, 14 ]
            [ CurveTo (Point 0 32) (Point 64 128) (Point 64 256)
            , CurveTo (Point 64 128) (Point -32 64) (Point -32 32)
            ]
        , testCharstring "vvcurveto 4"
            [ 32, 64, 96, 128, 160, -160, -128, -96, -64 ]
            [ 26, 14 ]
            [ CurveTo (Point 32 64) (Point 128 192) (Point 128 352)
            , CurveTo (Point 128 192) (Point 0 96) (Point 0 32)
            ]
        ]


hhcurveto =
    describe "hhcurveto"
        [ testCharstring "hhcurveto 1"
            [ 32, 64, 96, 128 ]
            [ 27, 14 ]
            [ CurveTo (Point 32 0) (Point 96 96) (Point 224 96)
            ]
        , testCharstring "hhcurveto 2"
            [ 32, 64, 96, 128, 160 ]
            [ 27, 14 ]
            [ CurveTo (Point 64 32) (Point 160 160) (Point 320 160)
            ]
        , testCharstring "hhcurveto 3"
            [ 32, 64, 96, 128, -128, -96, -64, -32 ]
            [ 27, 14 ]
            [ CurveTo (Point 32 0) (Point 96 96) (Point 224 96)
            , CurveTo (Point 96 96) (Point 0 32) (Point -32 32)
            ]
        , testCharstring "hhcurveto 4"
            [ 32, 64, 96, 128, 160, -160, -128, -96, -64 ]
            [ 27, 14 ]
            [ CurveTo (Point 64 32) (Point 160 160) (Point 320 160)
            , CurveTo (Point 160 160) (Point 32 64) (Point -32 64)
            ]
        ]


vhcurveto =
    describe "vhcurveto"
        [ testCharstring "vhcurveto 1"
            [ 32, 64, 96, 128 ]
            [ 30, 14 ]
            [ CurveTo (Point 0 32) (Point 64 128) (Point 192 128)
            ]
        , testCharstring "vhcurveto 2"
            [ 32, 64, 96, 128, 160 ]
            [ 30, 14 ]
            [ CurveTo (Point 0 32) (Point 64 128) (Point 192 288)
            ]
        , testCharstring "vhcurveto 3"
            [ 32, 64, 96, 128, 160, 192, -192, -160, -128, -96, -64, -32 ]
            [ 30, 14 ]
            [ CurveTo (Point 0 32) (Point 64 128) (Point 192 128)
            , CurveTo (Point 352 128) (Point 544 -64) (Point 544 -224)
            , CurveTo (Point 544 -352) (Point 448 -416) (Point 416 -416)
            ]
        , testCharstring "vhcurveto 4"
            [ 32, 64, 96, 128, 160, 192, -192, -160, -128, -96, -64, -32, 32 ]
            [ 30, 14 ]
            [ CurveTo (Point 0 32) (Point 64 128) (Point 192 128)
            , CurveTo (Point 352 128) (Point 544 -64) (Point 544 -224)
            , CurveTo (Point 544 -352) (Point 448 -416) (Point 416 -384)
            ]
        ]


hvcurveto =
    describe "hvcurveto"
        [ testCharstring "hvcurveto 1"
            [ 32, 64, 96, 128 ]
            [ 31, 14 ]
            [ CurveTo (Point 32 0) (Point 96 96) (Point 96 224)
            ]
        , testCharstring "hvcurveto 2"
            [ 32, 64, 96, 128, 160 ]
            [ 31, 14 ]
            [ CurveTo (Point 32 0) (Point 96 96) (Point 256 224)
            ]
        , testCharstring "hvcurveto 3"
            [ 32, 64, 96, 128, 160, 192, -192, -160, -128, -96, -64, -32 ]
            [ 31, 14 ]
            [ CurveTo (Point 32 0) (Point 96 96) (Point 96 224)
            , CurveTo (Point 96 384) (Point 288 192) (Point 128 192)
            , CurveTo (Point 0 192) (Point -96 128) (Point -96 96)
            ]
        , testCharstring "hvcurveto 4"
            [ 32, 64, 96, 128, 160, 192, -192, -160, -128, -96, -64, -32, 32 ]
            [ 31, 14 ]
            [ CurveTo (Point 32 0) (Point 96 96) (Point 96 224)
            , CurveTo (Point 96 384) (Point 288 192) (Point 128 192)
            , CurveTo (Point 0 192) (Point -96 128) (Point -64 96)
            ]
        ]


numberTest name op tape expected =
    test name <|
        \_ ->
            let
                bytes =
                    Encode.encode (Encode.sequence (List.map Encode.unsignedInt8 tape))
            in
            Decode.decode (Charstring.Number.decodeHelp op) bytes
                |> Expect.equal (Just expected)


number =
    describe "number"
        [ test "fixed conversion 1" <|
            \_ ->
                Charstring.Number.toInt (Fixed 0x01020304)
                    |> Expect.equal 258
        , test "fixed conversion 2" <|
            \_ ->
                Charstring.Number.toInt (Fixed 0x01040402)
                    |> Expect.equal 260
        , numberTest "number 1" 28 [ 1, 2 ] (Integer 0x0102)
        , numberTest "number 2" 32 [] (Integer -107)
        , numberTest "number 2b" 33 [] (Integer -106)
        , numberTest "number 3" 246 [] (Integer 107)
        , numberTest "number 4" 247 [ 0 ] (Integer 108)
        , numberTest "number 5" 250 [ 255 ] (Integer 1131)
        , numberTest "number 6" 251 [ 0 ] (Integer -108)
        , numberTest "number 7" 254 [ 255 ] (Integer -1131)
        , numberTest "number fixed" 255 [ 1, 2, 3, 4 ] (Fixed 0x01020304)
        , roundTripTest 0
        , roundTripTest 42
        , roundTripTest 400
        , roundTripTest 1000
        , roundTripTest -1000
        , roundTripTest 2000
        , roundTripTest -2000
        ]


roundTripTest n =
    test ("round trip " ++ String.fromInt n) <|
        \_ ->
            let
                bytes =
                    Encode.encode (Charstring.Number.encode n)
            in
            Decode.decode Charstring.Number.decode bytes
                |> Expect.equal (Just (Integer n))
