module Charstring.Internal exposing (Charstring, Operation(..), Point, Segment, Subroutines, decode, decodeSegments, initialSubroutines)

{-| The Charstring (CFF) internals


## Big Picture

A charstring is a list of numbers that encode drawing intructions like moveto, lineto, and curveto.

Because operators are normal numbers, we have to differentiate them from the arguments (which are also numbers).
This is done with a shifting scheme (in Charstring.Number)

The arguments come first and are pushed onto a stack (or really a dequeue, we mostly use first in first out).
When an operator is found, the arguments and the operator are bundled into a `Segment`.

A tricky thing is that while most operators only take these arguments, the masks can also chomp some bytes after
the operator token. This means that we have to decode segment-by-segment.

[spec]: https://www.adobe.com/content/dam/acom/en/devnet/font/pdfs/5177.Type2.pdf

-}

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Bytes.Encode
import Charstring.Number as Number exposing (Number)
import Utils exposing (exactly, keep)


type alias Subroutines =
    -- { offsetTable : Array ( Int, Int ), buffer : Bytes, offsetInBuffer : Int }
    Array Bytes


type alias Charstring =
    List Operation


type alias Segment =
    { operator : Int, arguments : List Number.Number }


{-| Decode a charstring as a list of segments

This may give a problem when there are masks in a subroutine.
The bytes after the mask operator (that contain the masking bits) are added as an argument to the next instruction

-}
decodeSegments : Int -> Decoder (List Segment)
decodeSegments bytesRemaining =
    Decode.loop ( bytesRemaining, [] ) decodeSegmentsHelp


decodeSegmentsHelp : ( Int, List Segment ) -> Decoder (Step ( Int, List Segment ) (List Segment))
decodeSegmentsHelp ( bytesRemaining, segments ) =
    if bytesRemaining <= 0 then
        Decode.succeed (Done (List.reverse segments))

    else
        decodeSegment bytesRemaining
            |> Decode.map (\( newBytesRemaining, segment ) -> Loop ( newBytesRemaining, segment :: segments ))


{-| Decode a single segment up to and including the operator token
-}
decodeSegment : Int -> Decoder ( Int, Segment )
decodeSegment bytesRemaining =
    Decode.loop ( bytesRemaining, [] ) decodeSegmentHelp


{-| Step the state forward until a Return or Endchar
-}
decodeSubroutine : Int -> State -> Decoder ( List (List Operation), State )
decodeSubroutine bytesRemaining state =
    Decode.loop ( bytesRemaining, [], state ) decodeSubroutineHelp
        |> Decode.map (\( operations, newState ) -> ( operations, newState ))


decodeSubroutineHelp : ( Int, List (List Operation), State ) -> Decoder (Step ( Int, List (List Operation), State ) ( List (List Operation), State ))
decodeSubroutineHelp ( bytesRemaining, accum, state ) =
    if bytesRemaining <= 0 then
        Decode.succeed (Done ( List.reverse accum, state ))

    else
        let
            addOperations : ( Int, List Operation, State ) -> Step ( Int, List (List Operation), State ) x
            addOperations ( newRemainder, operations, newState ) =
                Loop ( newRemainder, operations :: accum, newState )
        in
        decodeSegment bytesRemaining
            |> Decode.andThen
                (\( newBytesRemaining, segment ) ->
                    case segment.operator of
                        14 ->
                            Decode.succeed (Done ( List.reverse accum, { state | numbers = [] } ))

                        11 ->
                            Decode.succeed
                                (Done
                                    ( List.reverse accum
                                    , { state
                                        | numbers = state.numbers ++ List.map Number.toInt segment.arguments
                                      }
                                    )
                                )

                        _ ->
                            interpretSegment state ( newBytesRemaining, segment, [] )
                                |> Decode.map addOperations
                )


decodeSegmentHelp : ( Int, List Number ) -> Decoder (Step ( Int, List Number ) ( Int, { operator : Int, arguments : List Number.Number } ))
decodeSegmentHelp ( bytesRemaining, arguments ) =
    if bytesRemaining <= 0 then
        let
            _ =
                Debug.log "*** 0 bytes remaining" bytesRemaining
        in
        Decode.fail

    else
        Decode.unsignedInt8
            |> Decode.andThen
                (\byte ->
                    {-
                       let
                           _ =
                               if isNumberByte byte then
                                   Debug.log "byte"
                                       { byte = byte, current = bytesRemaining, delta = Number.sizeFromFirstByte byte, next = bytesRemaining - Number.sizeFromFirstByte byte, zarguments = arguments }

                               else if byte == 12 then
                                   Debug.log "2ope"
                                       { byte = byte, current = bytesRemaining, delta = 2, next = bytesRemaining - 2, zarguments = arguments }

                               else
                                   Debug.log "oper"
                                       { byte = byte, current = bytesRemaining, delta = 1, next = bytesRemaining - 1, zarguments = arguments }
                       in
                    -}
                    if isNumberByte byte then
                        Number.decodeHelp byte
                            |> Decode.map (\number -> Loop ( bytesRemaining - Number.sizeFromFirstByte byte, number :: arguments ))

                    else if byte == 12 then
                        Decode.unsignedInt8
                            |> Decode.map (\v -> Done ( bytesRemaining - 2, { operator = v + 3072, arguments = List.reverse arguments } ))

                    else
                        Decode.succeed (Done ( bytesRemaining - 1, { operator = byte, arguments = List.reverse arguments } ))
                )


decode : Int -> { global : Subroutines, local : Maybe Subroutines } -> Decoder Charstring
decode bytesRemaining { global, local } =
    let
        state =
            { initialState | global = global, local = local }

        _ =
            if False then
                Debug.log "---------------" bytesRemaining

            else
                0
    in
    decodeSubroutine bytesRemaining state
        |> Decode.map (List.concat << Tuple.first)


{-| Interpret a full segment to actually produce drawing instructions

Also counts the number of stems and reads the masking bytes correctly.

-}
interpretSegment : State -> ( Int, Segment, List Operation ) -> Decoder ( Int, List Operation, State )
interpretSegment state ( bytesRemaining, segment, operations ) =
    Decode.loop
        { state =
            { state
                | numbers = state.numbers ++ List.map Number.toInt segment.arguments
                , current = Nothing
                , length = List.length state.numbers + List.length segment.arguments
            }
        , bytesRemaining = bytesRemaining
        , operations = operations
        , opcode = segment.operator
        }
        interpretSegmentLoop


type alias LoopState =
    { opcode : Int, bytesRemaining : Int, operations : List Operation, state : State }


interpretSegmentLoop : LoopState -> Decoder (Step LoopState ( Int, List Operation, State ))
interpretSegmentLoop { opcode, bytesRemaining, operations, state } =
    interpretStatementHelp state opcode bytesRemaining
        |> Decode.andThen
            (\( newBytesRemaining, op, newState ) ->
                case newState.current of
                    Nothing ->
                        Decode.succeed (Done ( newBytesRemaining, List.reverse (List.reverse op ++ operations), newState ))

                    Just newOperator ->
                        Decode.succeed
                            (Loop
                                { opcode = newOperator
                                , bytesRemaining = newBytesRemaining
                                , operations = List.reverse op ++ operations
                                , state = newState
                                }
                            )
            )


interpretStatementHelp state opcode newBytesRemaining =
    let
        handleMask result =
            case result of
                Ok Nothing ->
                    let
                        _ =
                            Debug.log "hint mask caused a failure" opcode
                    in
                    if True then
                        Decode.fail

                    else
                        Decode.succeed ( newBytesRemaining, [], state )

                Ok (Just ( op, newState )) ->
                    Decode.succeed ( newBytesRemaining, op, newState )

                Err ( bytesChomped, op, newState ) ->
                    Decode.succeed ( newBytesRemaining - bytesChomped, op, newState )
    in
    case opcode of
        19 ->
            mask 19 HintMask state
                |> Decode.andThen handleMask

        20 ->
            mask 20 CounterMask state
                |> Decode.andThen handleMask

        _ ->
            case operator opcode state of
                Nothing ->
                    let
                        _ =
                            Debug.log "**** decodeWithOptionsHelp operator failed |||||||||||||||||||||||||||||||||||||||||||| =====================================" ( opcode, state.numbers, state )
                    in
                    if List.member opcode [] then
                        Decode.succeed ( newBytesRemaining, [], { state | numbers = [], current = Nothing } )

                    else
                        Debug.todo "crash"

                Just ( op, newState ) ->
                    Decode.succeed ( newBytesRemaining, op, newState )


{-| Is the current byte (the start of) a number?
-}
isNumberByte : Int -> Bool
isNumberByte byte =
    byte == 28 || byte >= 32


type alias Point =
    { x : Int, y : Int }


type Operation
    = CounterMask (List Int)
    | CurveTo Point Point Point
    | HintMask (List Int)
    | HStem Int Int
    | LineTo Point
    | MoveTo Point
    | VStem Int Int
    | Width Int


type alias State =
    { numbers : List Int
    , numStems : Int
    , hstem : Int
    , vstem : Int
    , current : Maybe Int
    , point : Point
    , length : Int
    , local : Maybe Subroutines
    , global : Subroutines
    , width : Maybe Int
    }


initialState : State
initialState =
    { numbers = []
    , numStems = 0
    , hstem = 0
    , vstem = 0
    , current = Nothing
    , point = Point 0 0
    , length = 0
    , local = Nothing
    , global = initialSubroutines
    , width = Nothing
    }


initialSubroutines : Subroutines
initialSubroutines =
    Array.empty


call : Subroutines -> Int -> State -> Maybe ( List Operation, State )
call subroutines index state =
    let
        size =
            Array.length subroutines

        actualIndex =
            if size < 1240 then
                index + 107

            else if size < 33900 then
                index + 1131

            else
                index + 32768
    in
    case Array.get actualIndex subroutines of
        Nothing ->
            Nothing

        Just buffer ->
            let
                decoder =
                    decodeSubroutine (Bytes.width buffer) state
            in
            case Decode.decode decoder buffer of
                Nothing ->
                    let
                        _ =
                            Debug.log "error in local subroutine decoding" ()
                    in
                    Nothing

                Just ( operations, newState ) ->
                    Just ( List.concat operations, newState )


mask : Int -> (List Int -> Operation) -> State -> Decoder (Result ( Int, List Operation, State ) (Maybe ( List Operation, State )))
mask op toOperation state =
    if not (List.isEmpty state.numbers) then
        -- handle implicit `vstem` operations
        case state.numbers of
            dx :: dStem :: rest ->
                let
                    x =
                        state.vstem + dx
                in
                Just
                    ( [ VStem x (x + dStem) ]
                    , { state
                        | numStems = 1 + state.numStems
                        , vstem = x + dStem
                        , current = Just op
                        , numbers = rest
                      }
                    )
                    |> Ok
                    |> Decode.succeed

            [ n ] ->
                if n == -20 || n == -21 || True then
                    Just
                        ( []
                        , { state
                            | numStems = 1 + state.numStems
                            , numbers = []
                          }
                        )
                        |> Ok
                        |> Decode.succeed

                else
                    let
                        _ =
                            Debug.log "invalid mask arguments" state.numbers
                    in
                    Decode.succeed (Ok Nothing)

            _ ->
                let
                    _ =
                        Debug.log "invalid mask arguments" state.numbers
                in
                Decode.succeed (Ok Nothing)

    else
        let
            numBytes =
                (state.numStems + 7) // 8
        in
        exactly numBytes Decode.unsignedInt8
            |> Decode.andThen
                (\bytes ->
                    Decode.succeed <|
                        Err ( numBytes, [ toOperation bytes ], { state | current = Nothing } )
                )


hstem : State -> Maybe ( List Operation, State )
hstem state =
    if odd (List.length state.numbers) then
        case state.numbers of
            width :: rest ->
                Just ( [ Width width ], { state | current = Just 1, numbers = rest } )

            _ ->
                Nothing

    else
        case state.numbers of
            y :: dy :: rest ->
                let
                    newY =
                        state.hstem + y
                in
                Just
                    ( [ HStem newY (newY + dy) ]
                    , { state
                        | numStems = state.numStems + 1
                        , hstem = newY + dy
                        , numbers = rest
                        , current =
                            if not (List.isEmpty rest) then
                                Just 1

                            else
                                Nothing
                      }
                    )

            _ ->
                Nothing


vstem : State -> Maybe ( List Operation, State )
vstem state =
    if odd (List.length state.numbers) then
        case state.numbers of
            width :: rest ->
                Just ( [ Width width ], { state | current = Just 3, numbers = rest } )

            _ ->
                Nothing

    else
        case state.numbers of
            x :: dx :: rest ->
                let
                    newX =
                        state.vstem + x
                in
                Just
                    ( [ VStem newX (newX + dx) ]
                    , { state
                        | numStems = state.numStems + 1
                        , vstem = newX + dx
                        , numbers = rest
                        , current =
                            if not (List.isEmpty rest) then
                                Just 3

                            else
                                Nothing
                      }
                    )

            _ ->
                Nothing


moveto state ( dx, dy ) newNumbers =
    let
        newPoint =
            { x = state.point.x + dx, y = state.point.y + dy }
    in
    Just
        ( [ MoveTo newPoint ]
        , { state
            | point = newPoint
            , current = Nothing
            , numbers = newNumbers
          }
        )


movetoWidth state op =
    case state.numbers of
        width :: rest ->
            Just
                ( [ Width width ]
                , { state
                    | current = Just op
                    , numbers = rest
                  }
                )

        _ ->
            Nothing


vmoveto : State -> Maybe ( List Operation, State )
vmoveto state =
    if List.length state.numbers == 2 then
        movetoWidth state 4

    else
        case state.numbers of
            dy :: rest ->
                moveto state ( 0, dy ) rest

            _ ->
                Nothing


hmoveto : State -> Maybe ( List Operation, State )
hmoveto state =
    if List.length state.numbers == 2 then
        movetoWidth state 22

    else
        case state.numbers of
            dx :: rest ->
                moveto state ( dx, 0 ) rest

            _ ->
                Nothing


rmoveto : State -> Maybe ( List Operation, State )
rmoveto state =
    if List.length state.numbers == 3 then
        movetoWidth state 21

    else
        case state.numbers of
            dx :: dy :: rest ->
                moveto state ( dx, dy ) rest

            _ ->
                Nothing


lineto : Int -> State -> ( Int, Int ) -> List Int -> ( List Operation, State )
lineto op state ( dx, dy ) newNumbers =
    let
        newPoint =
            { x = state.point.x + dx
            , y = state.point.y + dy
            }
    in
    ( [ LineTo newPoint ]
    , { state
        | numbers = newNumbers
        , point = newPoint
        , current =
            if List.isEmpty newNumbers then
                Nothing

            else
                Just op
      }
    )


rlineto : Int -> State -> Maybe ( List Operation, State )
rlineto op state =
    case state.numbers of
        dx :: dy :: rest ->
            lineto op state ( dx, dy ) rest
                |> Just

        _ ->
            Nothing


linetoX op state =
    case state.numbers of
        dx :: rest ->
            lineto op state ( dx, 0 ) rest
                |> Just

        _ ->
            Nothing


linetoY op state =
    case state.numbers of
        dy :: rest ->
            lineto op state ( 0, dy ) rest
                |> Just

        _ ->
            Nothing


hlineto : State -> Maybe ( List Operation, State )
hlineto state =
    if odd state.length == odd (List.length state.numbers) then
        linetoX 6 state

    else
        linetoY 6 state


vlineto : State -> Maybe ( List Operation, State )
vlineto state =
    if odd state.length == odd (List.length state.numbers) then
        linetoY 7 state

    else
        linetoX 7 state


{-| Call a local subroutine
-}
callsubr : State -> Maybe ( List Operation, State )
callsubr state =
    unSnoc state.numbers
        |> Maybe.andThen
            (\( index, previous ) ->
                case state.local of
                    Nothing ->
                        Nothing

                    Just subroutines ->
                        call subroutines index { state | numbers = previous }
            )


{-| Call a global subroutine
-}
callgsubr : State -> Maybe ( List Operation, State )
callgsubr state =
    case unSnoc state.numbers of
        Just ( index, newNumbers ) ->
            call state.global index { state | numbers = newNumbers }

        Nothing ->
            let
                _ =
                    Debug.log "callgsubr: no arguments" ()
            in
            Nothing



{-
   case state.numbers of
       index :: newNumbers ->
           call False state.global index { state | numbers = newNumbers }

       [] ->
           Nothing
-}


rcurveline state =
    if List.length state.numbers == 2 then
        rlineto 24 state

    else
        rrcurveto 24 state


rlinecurve state =
    if List.length state.numbers == 6 then
        rrcurveto 25 state

    else
        rlineto 25 state


curveto cursor dx dy dx2 dy2 dx3 dy3 =
    let
        a =
            { x = cursor.x + dx, y = cursor.y + dy }

        b =
            { x = a.x + dx2, y = a.y + dy2 }

        newPoint =
            { x = b.x + dx3, y = b.y + dy3 }
    in
    ( newPoint, [ CurveTo a b newPoint ] )


curvetoHelper op state rest ( newPoint, operation ) =
    Just
        ( operation
        , { state
            | point = newPoint
            , numbers = rest
            , current =
                if List.isEmpty rest then
                    Nothing

                else
                    Just op
          }
        )


rrcurveto : Int -> State -> Maybe ( List Operation, State )
rrcurveto op state =
    case state.numbers of
        dx :: dy :: dx2 :: dy2 :: dx3 :: dy3 :: rest ->
            curveto state.point dx dy dx2 dy2 dx3 dy3
                |> curvetoHelper op state rest

        _ ->
            Nothing


vvcurveto state =
    if odd (List.length state.numbers) then
        case state.numbers of
            dx :: dy :: dx2 :: dy2 :: d :: rest ->
                curveto state.point dx dy dx2 dy2 0 d
                    |> curvetoHelper 26 state rest

            _ ->
                Nothing

    else
        case state.numbers of
            dy :: dx2 :: dy2 :: d :: rest ->
                curveto state.point 0 dy dx2 dy2 0 d
                    |> curvetoHelper 26 state rest

            _ ->
                Nothing


hhcurveto state =
    if odd (List.length state.numbers) then
        case state.numbers of
            dy :: dx :: dx2 :: dy2 :: d :: rest ->
                curveto state.point dx dy dx2 dy2 d 0
                    |> curvetoHelper 27 state rest

            _ ->
                Nothing

    else
        case state.numbers of
            dx :: dx2 :: dy2 :: d :: rest ->
                curveto state.point dx 0 dx2 dy2 d 0
                    |> curvetoHelper 27 state rest

            _ ->
                Nothing


vhcurveto : State -> Maybe ( List Operation, State )
vhcurveto state =
    let
        ( _, cursor, curves ) =
            vhGeneralCase state
                |> vhSpecialCase1 state
                |> vhSpecialCase2 state
    in
    Just
        ( List.reverse curves
        , { state | numbers = [], current = Nothing, point = cursor }
        )


hvcurveto : State -> Maybe ( List Operation, State )
hvcurveto state =
    let
        ( _, cursor, curves ) =
            hvGeneralCase state
                |> hvSpecialCase1 state
                |> hvSpecialCase2 state
    in
    Just
        ( List.reverse curves
        , { state | numbers = [], current = Nothing, point = cursor }
        )


vhGeneralCase : State -> ( List Int, Point, List Operation )
vhGeneralCase state =
    let
        size =
            state.length

        countBezier =
            if (size |> modBy 4) == 1 then
                (size - 5) // 4

            else
                size // 4

        looper iteration cursor stack accum =
            if iteration >= countBezier then
                ( stack, cursor, accum )

            else if (iteration |> modBy 2) == 0 then
                case stack of
                    [] ->
                        ( stack, cursor, accum )

                    c0 :: c1 :: c2 :: c3 :: rest ->
                        let
                            p1 =
                                { x = cursor.x + 0, y = cursor.y + c0 }

                            p2 =
                                { x = p1.x + c1, y = p1.y + c2 }

                            p3 =
                                { x = p2.x + c3, y = p2.y + 0 }
                        in
                        looper (iteration + 1) p3 rest (CurveTo p1 p2 p3 :: accum)

                    _ ->
                        -- impossible
                        ( stack, cursor, accum )

            else
                case stack of
                    [] ->
                        ( stack, cursor, accum )

                    c0 :: c1 :: c2 :: c3 :: rest ->
                        let
                            p1 =
                                { x = cursor.x + c0, y = cursor.y + 0 }

                            p2 =
                                { x = p1.x + c1, y = p1.y + c2 }

                            p3 =
                                { x = p2.x + 0, y = p2.y + c3 }
                        in
                        looper (iteration + 1) p3 rest (CurveTo p1 p2 p3 :: accum)

                    _ ->
                        -- impossible
                        ( stack, cursor, accum )
    in
    looper 0 state.point state.numbers []


vhSpecialCase1 : State -> ( List Int, Point, List Operation ) -> ( List Int, Point, List Operation )
vhSpecialCase1 state ( stack, cursor, accum ) =
    if (state.length |> modBy 8) == 5 then
        case stack of
            c0 :: c1 :: c2 :: c3 :: c4 :: rest ->
                let
                    p1 =
                        { x = cursor.x + 0, y = cursor.y + c0 }

                    p2 =
                        { x = p1.x + c1, y = p1.y + c2 }

                    p3 =
                        { x = p2.x + c3, y = p2.y + c4 }
                in
                ( rest, p3, CurveTo p1 p2 p3 :: accum )

            _ ->
                -- impossible
                ( stack, cursor, accum )

    else
        ( stack, cursor, accum )


vhSpecialCase2 : State -> ( List Int, Point, List Operation ) -> ( List Int, Point, List Operation )
vhSpecialCase2 state ( stack, cursor, accum ) =
    if (state.length |> modBy 8) == 1 then
        case stack of
            c0 :: c1 :: c2 :: c3 :: c4 :: rest ->
                let
                    p1 =
                        { x = cursor.x + c0, y = cursor.y + 0 }

                    p2 =
                        { x = p1.x + c1, y = p1.y + c2 }

                    p3 =
                        { x = p2.x + c4, y = p2.y + c3 }
                in
                ( rest, p3, CurveTo p1 p2 p3 :: accum )

            _ ->
                -- impossible
                ( stack, cursor, accum )

    else
        ( stack, cursor, accum )


hvGeneralCase : State -> ( List Int, Point, List Operation )
hvGeneralCase state =
    let
        size =
            state.length

        countBezier =
            if (size |> modBy 4) == 1 then
                (size - 5) // 4

            else
                size // 4

        looper iteration cursor stack accum =
            if iteration >= countBezier then
                ( stack, cursor, accum )

            else if (iteration |> modBy 2) == 0 then
                case stack of
                    [] ->
                        ( stack, cursor, accum )

                    c0 :: c1 :: c2 :: c3 :: rest ->
                        let
                            p1 =
                                { x = cursor.x + c0, y = cursor.y + 0 }

                            p2 =
                                { x = p1.x + c1, y = p1.y + c2 }

                            p3 =
                                { x = p2.x + 0, y = p2.y + c3 }
                        in
                        looper (iteration + 1) p3 rest (CurveTo p1 p2 p3 :: accum)

                    _ ->
                        -- impossible
                        ( stack, cursor, accum )

            else
                case stack of
                    [] ->
                        ( stack, cursor, accum )

                    c0 :: c1 :: c2 :: c3 :: rest ->
                        let
                            p1 =
                                { x = cursor.x + 0, y = cursor.y + c0 }

                            p2 =
                                { x = p1.x + c1, y = p1.y + c2 }

                            p3 =
                                { x = p2.x + c3, y = p2.y + 0 }
                        in
                        looper (iteration + 1) p3 rest (CurveTo p1 p2 p3 :: accum)

                    _ ->
                        -- impossible
                        ( stack, cursor, accum )
    in
    looper 0 state.point state.numbers []


hvSpecialCase1 : State -> ( List Int, Point, List Operation ) -> ( List Int, Point, List Operation )
hvSpecialCase1 state ( stack, cursor, accum ) =
    if (state.length |> modBy 8) == 5 then
        case stack of
            c0 :: c1 :: c2 :: c3 :: c4 :: rest ->
                let
                    p1 =
                        { x = cursor.x + c0, y = cursor.y }

                    p2 =
                        { x = p1.x + c1, y = p1.y + c2 }

                    p3 =
                        { x = p2.x + c4, y = p2.y + c3 }
                in
                ( rest, p3, CurveTo p1 p2 p3 :: accum )

            _ ->
                -- impossible
                ( stack, cursor, accum )

    else
        ( stack, cursor, accum )


hvSpecialCase2 : State -> ( List Int, Point, List Operation ) -> ( List Int, Point, List Operation )
hvSpecialCase2 state ( stack, cursor, accum ) =
    if (state.length |> modBy 8) == 1 then
        case stack of
            c0 :: c1 :: c2 :: c3 :: c4 :: rest ->
                let
                    p1 =
                        { x = cursor.x, y = cursor.y + c0 }

                    p2 =
                        { x = p1.x + c1, y = p1.y + c2 }

                    p3 =
                        { x = p2.x + c3, y = p2.y + c4 }
                in
                ( rest, p3, CurveTo p1 p2 p3 :: accum )

            _ ->
                -- impossible
                ( stack, cursor, accum )

    else
        ( stack, cursor, accum )


operator : Int -> State -> Maybe ( List Operation, State )
operator code state =
    case code of
        1 ->
            hstem state

        18 ->
            hstem state

        3 ->
            vstem state

        23 ->
            vstem state

        4 ->
            vmoveto state

        5 ->
            rlineto 5 state

        6 ->
            hlineto state

        7 ->
            vlineto state

        8 ->
            rrcurveto 8 state

        10 ->
            callsubr state

        -- 11, 14, 19 and 20 are all handled elsewhere
        21 ->
            rmoveto state

        22 ->
            hmoveto state

        24 ->
            rcurveline state

        25 ->
            rlinecurve state

        26 ->
            vvcurveto state

        27 ->
            hhcurveto state

        29 ->
            callgsubr state

        30 ->
            vhcurveto state

        31 ->
            hvcurveto state
                |> Maybe.map (\( op, newState ) -> ( op, { newState | numbers = [] } ))

        _ ->
            let
                _ =
                    Debug.log "unkown charstring operator" code
            in
            Nothing


even : Int -> Bool
even x =
    (x |> modBy 2) == 0


odd : Int -> Bool
odd x =
    (x |> modBy 2) /= 0


last : List a -> Maybe a
last list =
    case list of
        [ x ] ->
            Just x

        x :: xs ->
            last xs

        [] ->
            Nothing


unSnoc : List a -> Maybe ( a, List a )
unSnoc list =
    let
        go remainder accum =
            case remainder of
                [ x ] ->
                    Just ( x, List.reverse accum )

                [] ->
                    Nothing

                x :: xs ->
                    go xs (x :: accum)
    in
    go list []
