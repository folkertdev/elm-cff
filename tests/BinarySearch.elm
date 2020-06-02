module BinarySearch exposing (suite)

import Array
import Array.Extra
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite =
    Test.describe "array binary search"
        [ binarySearchBy
        , binarySearch
        , binarySearchByKey
        ]


binarySearchBy =
    let
        array =
            Array.fromList [ 0, 1, 1, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55 ]
    in
    Test.describe "binarySearchBy"
        [ test "seek 13" <|
            \_ ->
                Array.Extra.binarySearchBy (\value -> compare value 13) array
                    |> Expect.equal (Ok 9)
        , test "seek 4" <|
            \_ ->
                Array.Extra.binarySearchBy (\value -> compare value 4) array
                    |> Expect.equal (Err 7)
        , test "seek 100" <|
            \_ ->
                Array.Extra.binarySearchBy (\value -> compare value 100) array
                    |> Expect.equal (Err 13)
        , test "if result is Ok for 1, then index in 1..=4" <|
            \_ ->
                case Array.Extra.binarySearchBy (\value -> compare value 1) array of
                    Ok i ->
                        if i >= 1 && i <= 4 then
                            Expect.pass

                        else
                            Expect.fail "wrong result found"

                    Err _ ->
                        Expect.fail "1 should be found"
        ]


binarySearch =
    let
        array =
            Array.fromList [ 0, 1, 1, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55 ]
    in
    Test.describe "binarySearch"
        [ test "seek 13" <|
            \_ ->
                Array.Extra.binarySearch 13 array
                    |> Expect.equal (Ok 9)
        , test "seek 4" <|
            \_ ->
                Array.Extra.binarySearch 4 array
                    |> Expect.equal (Err 7)
        , test "seek 100" <|
            \_ ->
                Array.Extra.binarySearch 100 array
                    |> Expect.equal (Err 13)
        , test "if result is Ok for 1, then index in 1..=4" <|
            \_ ->
                case Array.Extra.binarySearch 1 array of
                    Ok i ->
                        if i >= 1 && i <= 4 then
                            Expect.pass

                        else
                            Expect.fail "wrong result found"

                    Err _ ->
                        Expect.fail "1 should be found"
        ]


binarySearchByKey =
    let
        array =
            Array.fromList [ ( 0, 0 ), ( 2, 1 ), ( 4, 1 ), ( 5, 1 ), ( 3, 1 ), ( 1, 2 ), ( 2, 3 ), ( 4, 5 ), ( 5, 8 ), ( 3, 13 ), ( 1, 21 ), ( 2, 34 ), ( 4, 55 ) ]
    in
    Test.describe "binarySearchByKey"
        [ test "seek 13" <|
            \_ ->
                Array.Extra.binarySearchByKey Tuple.second 13 array
                    |> Expect.equal (Ok 9)
        , test "seek 4" <|
            \_ ->
                Array.Extra.binarySearchByKey Tuple.second 4 array
                    |> Expect.equal (Err 7)
        , test "seek 100" <|
            \_ ->
                Array.Extra.binarySearchByKey Tuple.second 100 array
                    |> Expect.equal (Err 13)
        , test "if result is Ok for 1, then index in 1..=4" <|
            \_ ->
                case Array.Extra.binarySearchByKey Tuple.second 1 array of
                    Ok i ->
                        if i >= 1 && i <= 4 then
                            Expect.pass

                        else
                            Expect.fail "wrong result found"

                    Err _ ->
                        Expect.fail "1 should be found"
        ]
