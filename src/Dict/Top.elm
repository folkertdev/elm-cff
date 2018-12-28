module Dict.Top exposing (Cid, Top, decode)

import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Number exposing (Number, Operator(..), entry)


decode : Int -> Decoder Top
decode remainingBytes =
    -- TODO this -1 is pretty arbitrary
    -- but seems to work robustly
    Decode.loop ( remainingBytes - 1, defaultTop, defaultCid ) decodeHelp


decodeHelp : ( Int, Top, Cid ) -> Decoder (Step ( Int, Top, Cid ) Top)
decodeHelp ( remainingBytes, top, cid ) =
    if remainingBytes <= 0 then
        if cid.ros /= ( 0, 0, 0 ) then
            Decode.succeed (Done { top | cid = Just cid })

        else
            Decode.succeed (Done top)

    else
        entry
            |> Decode.andThen
                (\{ operator, numbers, size } ->
                    case operator of
                        Operator _ opcode ->
                            if opcode >= 3102 && opcode <= 3110 then
                                case updateCid operator cid numbers of
                                    Nothing ->
                                        let
                                            _ =
                                                Debug.log "decodeHelp failed apply cid operator failed" ()
                                        in
                                        Decode.fail

                                    Just newCid ->
                                        Decode.succeed (Loop ( remainingBytes - size, top, newCid ))

                            else
                                case updateTop operator top numbers of
                                    Nothing ->
                                        let
                                            _ =
                                                Debug.log "decodeHelp failed apply operator failed" ( operator, numbers, ( top, remainingBytes, size ) )
                                        in
                                        Decode.fail

                                    Just newTop ->
                                        Decode.succeed (Loop ( remainingBytes - size, newTop, cid ))
                )


type alias Cid =
    { ros : ( Int, Int, Int )
    , font_version : Float
    , font_revision : Float
    , font_type : Int
    , count : Int
    , uid_base : Maybe Int
    , fd_array : Int
    , fd_select : Int
    , font_name : Maybe Int
    }


defaultCid =
    { ros = ( 0, 0, 0 )
    , font_version = 0
    , font_revision = 0
    , font_type = 0
    , count = 8720
    , uid_base = Nothing
    , fd_array = 0
    , fd_select = 0
    , font_name = Nothing
    }


type alias FontMatrix =
    { a : Float
    , b : Float
    , c : Float
    , d : Float
    , e : Float
    , f : Float
    }


defaultFontMatrix =
    FontMatrix 0.001 0.0 0.0 0.001 0.0 0.0


type alias Point =
    { x : Int, y : Int }


type alias FontBoundingBox =
    { bottomLeft : Point
    , topRight : Point
    }


defaultFontBoundingBox =
    { bottomLeft = Point 0 0
    , topRight = Point 0 0
    }


type alias Top =
    { -- The string ID for the version of the font.
      version : Maybe Int
    , -- The string ID for the notice of the font.
      notice : Maybe Int
    , -- The string ID for the copyright of the font.
      copyright : Maybe Int
    , -- The string ID for the full name of the font.
      full_name : Maybe Int
    , -- The string ID for the family name of the font.
      family_name : Maybe Int
    , -- The string ID for the weight of the font.
      weight : Maybe Int
    , -- Whether the font is a monospace font.
      is_fixed_pitch : Bool
    , italic_angle : Int
    , underline_position : Int
    , underline_thickness : Int
    , paint_type : Int
    , charstring_type : Int
    , font_matrix : FontMatrix
    , unique_id : Maybe Int
    , font_bounding_box : FontBoundingBox
    , stroke_width : Int
    , xuid : Maybe (List Number)
    , charset : Int
    , encoding : Int
    , charstrings : Maybe Int
    , private : Maybe { size : Int, offset : Int }
    , synthetic_base : Maybe Int
    , post_script : Maybe Int
    , base_font_name : Maybe Int
    , base_font_blend : Maybe (List Number)
    , cid : Maybe Cid
    }


defaultTop =
    { version = Nothing
    , notice = Nothing
    , copyright = Nothing
    , full_name = Nothing
    , family_name = Nothing
    , weight = Nothing
    , is_fixed_pitch = False
    , italic_angle = 0
    , underline_position = -100
    , underline_thickness = 50
    , paint_type = 0
    , charstring_type = 2
    , font_matrix = defaultFontMatrix
    , unique_id = Nothing
    , font_bounding_box = defaultFontBoundingBox
    , stroke_width = 0
    , xuid = Nothing
    , charset = 0
    , encoding = 0
    , charstrings = Nothing
    , private = Nothing
    , synthetic_base = Nothing
    , post_script = Nothing
    , base_font_name = Nothing
    , base_font_blend = Nothing
    , cid = Nothing
    }


updateCid : Operator -> Cid -> List Number -> Maybe Cid
updateCid operator cid numbers =
    let
        (Operator _ opcode) =
            operator

        withHeadInt f =
            case numbers of
                first :: rest ->
                    Just (f (Number.toInt first))

                [] ->
                    Nothing

        withHeadFloat f =
            case numbers of
                first :: rest ->
                    Just (f (Number.toFloat first))

                [] ->
                    Nothing
    in
    case opcode of
        3102 ->
            case numbers of
                [ registry, ordering, supplement ] ->
                    let
                        ros =
                            ( Number.toInt registry
                            , Number.toInt ordering
                            , Number.toInt supplement
                            )
                    in
                    Just { cid | ros = ros }

                _ ->
                    Nothing

        3103 ->
            withHeadFloat (\v -> { cid | font_version = v })

        3104 ->
            withHeadFloat (\v -> { cid | font_revision = v })

        3105 ->
            withHeadInt (\v -> { cid | font_type = v })

        3106 ->
            withHeadInt (\v -> { cid | count = v })

        3107 ->
            withHeadInt (\v -> { cid | uid_base = Just v })

        3108 ->
            withHeadInt (\v -> { cid | fd_array = v })

        3109 ->
            withHeadInt (\v -> { cid | fd_select = v })

        3110 ->
            withHeadInt (\v -> { cid | font_name = Just v })

        _ ->
            Debug.log "cid fell through" Nothing


updateTop : Operator -> Top -> List Number -> Maybe Top
updateTop operator top numbers =
    let
        (Operator _ opcode) =
            operator

        withHeadInt f =
            case numbers of
                first :: rest ->
                    Just (f (Number.toInt first))

                [] ->
                    Nothing

        withHeadFloat f =
            case numbers of
                first :: rest ->
                    Just (f (Number.toFloat first))

                [] ->
                    Nothing
    in
    case opcode of
        0 ->
            withHeadInt (\v -> { top | version = Just v })

        1 ->
            withHeadInt (\v -> { top | notice = Just v })

        2 ->
            withHeadInt (\v -> { top | full_name = Just v })

        3 ->
            withHeadInt (\v -> { top | family_name = Just v })

        4 ->
            withHeadInt (\v -> { top | weight = Just v })

        5 ->
            case List.map Number.toInt numbers of
                [ x1, y1, x2, y2 ] ->
                    Just { top | font_bounding_box = FontBoundingBox (Point x1 y1) (Point x2 y2) }

                _ ->
                    Nothing

        14 ->
            withHeadInt (\v -> { top | xuid = Just numbers })

        15 ->
            withHeadInt (\v -> { top | charset = v })

        16 ->
            withHeadInt (\v -> { top | encoding = v })

        17 ->
            withHeadInt (\v -> { top | charstrings = Just v })

        18 ->
            case List.map Number.toInt numbers of
                [ size, offset ] ->
                    Just { top | private = Just { size = size, offset = offset } }

                _ ->
                    Nothing

        3072 ->
            withHeadInt (\v -> { top | copyright = Just v })

        3073 ->
            withHeadInt (\v -> { top | is_fixed_pitch = v /= 0 })

        3074 ->
            withHeadInt (\v -> { top | italic_angle = v })

        3075 ->
            withHeadInt (\v -> { top | underline_position = v })

        3076 ->
            withHeadInt (\v -> { top | underline_thickness = v })

        3077 ->
            withHeadInt (\v -> { top | paint_type = v })

        3078 ->
            withHeadInt (\v -> { top | charstring_type = v })

        3079 ->
            case List.map Number.toFloat numbers of
                [ a, b, c, d, e, f ] ->
                    Just { top | font_matrix = FontMatrix a b c d e f }

                _ ->
                    Nothing

        3080 ->
            withHeadInt (\v -> { top | stroke_width = v })

        3092 ->
            withHeadInt (\v -> { top | synthetic_base = Just v })

        3093 ->
            withHeadInt (\v -> { top | post_script = Just v })

        3094 ->
            withHeadInt (\v -> { top | base_font_name = Just v })

        3095 ->
            withHeadInt (\v -> { top | base_font_blend = Just numbers })

        _ ->
            Debug.log "top fell through" Nothing
