module Array.Extra exposing (binarySearch, binarySearchBy, binarySearchByKey)

{-| Array binary search based on the [implementation in the rust stdlib](https://doc.rust-lang.org/src/core/slice/mod.rs.html#1409-1432)
-}

import Array exposing (Array)


binarySearchBy : (a -> Order) -> Array a -> Result Int Int
binarySearchBy toComparable array =
    let
        go size base =
            if size > 1 then
                let
                    half =
                        size // 2

                    mid =
                        base + half
                in
                case Array.get mid array of
                    Nothing ->
                        -- mid is always in [0, size), that means mid is >= 0 and < size.
                        -- mid >= 0: by definition
                        -- mid < size: mid = size / 2 + size / 4 + size / 8 ...
                        Err -1

                    Just currentValue ->
                        go (size - half)
                            (if toComparable currentValue == GT then
                                base

                             else
                                mid
                            )

            else
                case Array.get base array of
                    Nothing ->
                        -- impossible, base is always [0, size) because base <= mid
                        Err -1

                    Just currentValue ->
                        case toComparable currentValue of
                            LT ->
                                Err (base + 1)

                            EQ ->
                                Ok base

                            GT ->
                                Err base
    in
    go (Array.length array) 0


binarySearchByKey : (a -> comparable) -> comparable -> Array a -> Result Int Int
binarySearchByKey toKey target array =
    binarySearchBy (\element -> compare (toKey element) target) array


binarySearch : comparable -> Array comparable -> Result Int Int
binarySearch target array =
    binarySearchBy (\element -> compare element target) array
