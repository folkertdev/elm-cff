module Dict.Private exposing (Private, default, decode)

{-|

@docs Private, default, decode

-}

import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Decode.Extra
import Dict.Operator exposing (Argument, Operator, argumentToFloat, argumentToInt)


{-| The private dict.
-}
type alias Private =
    { blues : List Argument
    , other_blues : List Argument
    , family_blues : List Argument
    , family_other_blues : List Argument
    , blue_scale : Float
    , blue_shift : Float
    , blue_fuzz : Float
    , std_hw : Maybe Float
    , std_vw : Maybe Float
    , stem_snap_h : List Argument
    , stem_snap_v : List Argument
    , force_bold : Bool
    , language_group : Int
    , expansion_factor : Float
    , initial_random_seed : Int
    , subroutines : Maybe Int
    , default_width_x : Int
    , nominal_width_x : Int
    }


{-| The default values for the `Private` dict.
-}
default : Private
default =
    { blues = []
    , other_blues = []
    , family_blues = []
    , family_other_blues = []
    , blue_scale = 0.039625
    , blue_shift = 7
    , blue_fuzz = 1
    , std_hw = Nothing
    , std_vw = Nothing
    , stem_snap_h = []
    , stem_snap_v = []
    , force_bold = False
    , language_group = 0
    , expansion_factor = 0.06
    , initial_random_seed = 0
    , subroutines = Nothing
    , default_width_x = 0
    , nominal_width_x = 0
    }


{-| Decode the private dict given its size in bytes
-}
decode : Int -> Decoder Private
decode remainingBytes =
    -- TODO error in counting the bytes?
    Decode.loop ( remainingBytes - 2, default ) decodeHelp


decodeHelp ( remainingBytes, private ) =
    if remainingBytes <= 0 then
        Decode.succeed (Done private)

    else
        Dict.Operator.decode
            |> Decode.andThen
                (\({ opcode, arguments, size } as operator) ->
                    case applyOperator operator arguments private of
                        Nothing ->
                            -- NOTE there is probably something weird with counting the bytes going on here
                            Decode.Extra.failWith "decodeHelp failed apply operator failed"

                        Just newPrivate ->
                            Decode.succeed (Loop ( remainingBytes - size, newPrivate ))
                )


applyOperator : Operator -> List Argument -> Private -> Maybe Private
applyOperator { opcode } arguments private =
    let
        withHead f =
            Maybe.map f (List.head arguments)
    in
    -- TODO rust code uses first here
    case opcode of
        6 ->
            Just { private | blues = arguments }

        7 ->
            Just { private | other_blues = arguments }

        8 ->
            Just { private | family_blues = arguments }

        9 ->
            Just { private | family_other_blues = arguments }

        10 ->
            Just { private | std_hw = List.head arguments |> Maybe.map argumentToFloat }

        11 ->
            Just { private | std_vw = List.head arguments |> Maybe.map argumentToFloat }

        19 ->
            Just { private | subroutines = List.head arguments |> Maybe.map argumentToInt }

        20 ->
            withHead (\v -> { private | default_width_x = v |> argumentToInt })

        21 ->
            withHead (\v -> { private | nominal_width_x = v |> argumentToInt })

        --
        3081 ->
            withHead (\v -> { private | blue_scale = v |> argumentToFloat })

        3082 ->
            withHead (\v -> { private | blue_shift = v |> argumentToFloat })

        3083 ->
            withHead (\v -> { private | blue_fuzz = v |> argumentToFloat })

        3084 ->
            Just { private | stem_snap_h = arguments }

        3085 ->
            Just { private | stem_snap_v = arguments }

        3086 ->
            withHead (\v -> { private | force_bold = argumentToInt v /= 0 })

        3089 ->
            withHead (\v -> { private | language_group = v |> argumentToInt })

        3090 ->
            withHead (\v -> { private | expansion_factor = v |> argumentToFloat })

        3091 ->
            withHead (\v -> { private | initial_random_seed = v |> argumentToInt })

        _ ->
            Nothing
