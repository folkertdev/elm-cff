module Dict.Private exposing (Private, decode, defaultPrivate)

import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Number exposing (Entry, Number, Operator(..), entry)


type alias Numbers =
    List Number.Number


type alias Private =
    { blues : Maybe Numbers
    , other_blues : Maybe Numbers
    , family_blues : Maybe Numbers
    , family_other_blues : Maybe Numbers
    , blue_scale : Float
    , blue_shift : Float
    , blue_fuzz : Float
    , std_hw : Maybe Float
    , std_vw : Maybe Float
    , stem_snap_h : Maybe Numbers
    , stem_snap_v : Maybe Numbers
    , force_bold : Bool
    , language_group : Int
    , expansion_factor : Float
    , initial_random_seed : Int
    , subroutines : Maybe Int
    , default_width_x : Int
    , nominal_width_x : Int
    }


defaultPrivate =
    { blues = Nothing
    , other_blues = Nothing
    , family_blues = Nothing
    , family_other_blues = Nothing
    , blue_scale = 0.039625
    , blue_shift = 7
    , blue_fuzz = 1
    , std_hw = Nothing
    , std_vw = Nothing
    , stem_snap_h = Nothing
    , stem_snap_v = Nothing
    , force_bold = False
    , language_group = 0
    , expansion_factor = 0.06
    , initial_random_seed = 0
    , subroutines = Nothing
    , default_width_x = 0
    , nominal_width_x = 0
    }


decode : Int -> Decoder Private
decode remainingBytes =
    -- TODO error in counting the bytes?
    Decode.loop ( remainingBytes - 2, defaultPrivate ) decodeHelp


decodeHelp ( remainingBytes, private ) =
    if remainingBytes <= 0 then
        Decode.succeed (Done private)

    else
        entry
            |> Decode.andThen
                (\({ operator, numbers, size } as et) ->
                    let
                        _ =
                            Debug.log "entry" et
                    in
                    case applyOperator operator numbers private of
                        Nothing ->
                            -- NOTE there is probably something weird with counting the bytes going on here
                            let
                                _ =
                                    Debug.log "decodeHelp failed apply operator failed" ( ( operator, numbers ), size, remainingBytes )
                            in
                            Decode.fail

                        Just newPrivate ->
                            Decode.succeed (Loop ( remainingBytes - size, newPrivate ))
                )


applyOperator : Operator -> List Number -> Private -> Maybe Private
applyOperator (Operator _ operator) numbers private =
    let
        withHead f =
            Maybe.map f (List.head numbers)
    in
    -- TODO rust code uses first here
    case operator of
        6 ->
            Just { private | blues = Just numbers }

        7 ->
            Just { private | other_blues = Just numbers }

        8 ->
            Just { private | family_blues = Just numbers }

        9 ->
            Just { private | family_other_blues = Just numbers }

        10 ->
            Just { private | std_hw = List.head numbers |> Maybe.map Number.toFloat }

        11 ->
            Just { private | std_vw = List.head numbers |> Maybe.map Number.toFloat }

        19 ->
            Just { private | subroutines = List.head numbers |> Maybe.map Number.toInt }

        20 ->
            withHead (\v -> { private | default_width_x = v |> Number.toInt })

        21 ->
            withHead (\v -> { private | nominal_width_x = v |> Number.toInt })

        --
        3081 ->
            withHead (\v -> { private | blue_scale = v |> Number.toFloat })

        3082 ->
            withHead (\v -> { private | blue_shift = v |> Number.toFloat })

        3083 ->
            withHead (\v -> { private | blue_fuzz = v |> Number.toFloat })

        3084 ->
            Just { private | stem_snap_h = Just numbers }

        3085 ->
            Just { private | stem_snap_v = Just numbers }

        3086 ->
            withHead (\v -> { private | force_bold = Number.toInt v /= 0 })

        3089 ->
            withHead (\v -> { private | language_group = v |> Number.toInt })

        3090 ->
            withHead (\v -> { private | expansion_factor = v |> Number.toFloat })

        3091 ->
            withHead (\v -> { private | initial_random_seed = v |> Number.toInt })

        _ ->
            Nothing
