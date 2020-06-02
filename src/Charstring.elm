module Charstring exposing
    ( Charstring
    , Operation(..), Point
    , decode, Subroutines
    )

{-| A charstring is a sequence of numbers that encodes the shape of a glyph with drawing and layout operators like moveto, lineto, and curveto.

Because both operators and their arguments are numbers, we have to differentiate the two.
The operators use the numbers 0..31 (as unsignedInt8) and arguments use all other values.
To be able to use 0..31 as arguments too, the arguments are shifted (specifics are in `Charstring.Number`).

The arguments come first and are pushed onto a stack (or really a dequeue, we mostly use first in first out).
When an operator is found, the arguments and the operator are bundled together.

A tricky thing is that while most operators only take these arguments, the mask operators can also chomp some bytes after
the operator token. This means that we have to decode from left to right, one full operation at a time.

@docs Charstring

@docs Operation, Point

@docs decode, Subroutines

-}

import Array exposing (Array)
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Charstring.Number as Number exposing (Number)
import Decode.Extra


{-| The `Charstring` is what defines the actual shape of a glyph. It is a list of drawing instructions (like moveto, lineto, and curveto).
-}
type alias Charstring =
    List Operation


{-| A 2D point with integer coordinates
-}
type alias Point =
    { x : Int
    , y : Int
    }


{-| The drawing operations. For the full details see the [charstring 2 spec][spec].

  - **HintMask** and **CounterMask**: masks to turn on stem hints
  - **HStem** and **VStem** definition of stems
  - **Width**: optional value that gives the width of the charstring
  - **MoveTo**: move the drawing cursor
  - **LineTo** and **CurveTo**: draw a line (resp. a cubic curve) from the current drawing cursor

[spec]: https://www.adobe.com/content/dam/acom/en/devnet/font/pdfs/5177.Type2.pdf

-}
type Operation
    = HintMask (List Int)
    | CounterMask (List Int)
    | HStem Int Int
    | VStem Int Int
    | Width Int
    | MoveTo Point
    | LineTo Point
    | CurveTo Point Point Point


{-| Subroutines are initially stored as an array of `Bytes` objects. Global subroutines are a CFF table, local subroutines are part of the PRIVATE table.

At any point between operators in a charstring, a subroutine can be invoked.
Subroutines are pieces of charstrings that occur often and are therfore abstracted to save space.

Subroutines can be either global (used by all fonts in a fontset) or local (used only in this particular font).
Decoding subroutines correctly is tricky because the decoding depends on the current `State`, in particular
the arguments on the stack (`State.argumentStack`).

The solution I've settled on is to store the subroutines as `Bytes`, and when a subroutine is called, we evaluate the normal charstring decoder with the subroutine bytes.
The storage of the subroutines in this way is cheap, because a `Bytes` slice really only stores an offset and a length. It doesn't copy the underlying `Bytes`.

-}
type alias Subroutines =
    Array Bytes


{-| A list of arguments and an operator code
-}
type alias Segment =
    { operator : Int, arguments : List Number.Number }


{-| Decode a single segment up to and including the operator token
-}
decodeSegment : Decoder Segment
decodeSegment =
    Decode.loop [] decodeSegmentHelp


{-| Step the state forward until a Return or Endchar

This decoder makes heavy use of counting the number of decoded bytes to not read past the end of the charstring.
A charstring can end in 4 different opcodes

  - code 11: subroutine return
  - code 14: endchar
  - code 10: local subroutine call
  - code 29: global subroutine call

In the first 2 cases we're definitely done, but the latter two are difficult.
They can occur as the final operator, but don't need to. So just stopping when they are encountered
would be incorrect.

The decoding of the called subroutine signals that the end was reached, and stops reading more bytes.

Two other codes are special:

  - code 19: hintmask
  - code 20: cntrmask

The masking operators can read bytes after the operator. This complicates decoding considerably,
because we can't split a charstring on the operator bytes (that would but the mask bits into the argument stack of the succeeding operator).

-}
decodeSubroutine : State -> Decoder (EndReached ( List Charstring, State ))
decodeSubroutine state =
    Decode.loop ( [], state ) decodeSubroutineHelp


type EndReached a
    = EndReached a
    | EndNotReached a


decodeSubroutineHelp : ( List Charstring, State ) -> Decoder (Step ( List Charstring, State ) (EndReached ( List Charstring, State )))
decodeSubroutineHelp ( accum, state ) =
    let
        addOperations : ( List Operation, State ) -> Step ( List Charstring, State ) x
        addOperations ( operations, newState ) =
            Loop ( operations :: accum, newState )

        handleSubroutineCall :
            Segment
            -> (State -> Maybe (EndReached ( Charstring, State )))
            -> Decoder (Step ( List Charstring, State ) (EndReached ( List Charstring, State )))
        handleSubroutineCall segment subroutineCall =
            case subroutineCall { state | argumentStack = state.argumentStack ++ List.map Number.toInt segment.arguments } of
                Nothing ->
                    Decode.fail

                Just (EndReached ( operations, newState )) ->
                    Decode.succeed
                        (Done
                            (EndReached
                                ( List.reverse (operations :: accum)
                                , newState
                                )
                            )
                        )

                Just (EndNotReached ( operations, newState )) ->
                    Decode.succeed
                        (Loop
                            ( operations :: accum
                            , newState
                            )
                        )
    in
    decodeSegment
        |> Decode.andThen
            (\segment ->
                let
                    newState =
                        { state
                            | argumentStack = state.argumentStack ++ List.map Number.toInt segment.arguments
                            , length = List.length state.argumentStack + List.length segment.arguments
                        }
                in
                case segment.operator of
                    10 ->
                        handleSubroutineCall segment callsubr

                    29 ->
                        handleSubroutineCall segment callgsubr

                    14 ->
                        Decode.succeed (Done (EndReached ( List.reverse accum, { state | argumentStack = [] } )))

                    11 ->
                        Decode.succeed
                            (Done
                                (EndNotReached
                                    ( List.reverse accum
                                    , newState
                                    )
                                )
                            )

                    19 ->
                        mask segment.operator HintMask newState
                            |> Decode.map addOperations

                    20 ->
                        mask segment.operator CounterMask newState
                            |> Decode.map addOperations

                    _ ->
                        interpretSegment state segment
                            |> Decode.map addOperations
            )


decodeSegmentHelp : List Number -> Decoder (Step (List Number) { operator : Int, arguments : List Number.Number })
decodeSegmentHelp arguments =
    Decode.unsignedInt8
        |> Decode.andThen
            (\byte ->
                if isNumberByte byte then
                    Number.decodeHelp byte
                        |> Decode.map (\number -> Loop (number :: arguments))

                else if byte == 12 then
                    Decode.unsignedInt8
                        |> Decode.map (\v -> Done { operator = v + 3072, arguments = List.reverse arguments })

                else
                    Decode.succeed (Done { operator = byte, arguments = List.reverse arguments })
            )


{-| Decode a `Charstring` given global and local subroutines.
-}
decode : { global : Subroutines, local : Maybe Subroutines } -> Decoder Charstring
decode { global, local } =
    let
        state =
            { initialState | global = global, local = local }
    in
    decodeSubroutine state
        |> Decode.andThen
            (\result ->
                case result of
                    EndReached ( operations, _ ) ->
                        Decode.succeed (List.concat operations)

                    EndNotReached _ ->
                        Decode.fail
            )


{-| Interpret a full segment to actually produce drawing instructions

Also counts the number of stems and reads the masking bytes correctly.

-}
interpretSegment : State -> Segment -> Decoder ( List Operation, State )
interpretSegment state segment =
    decodeDrawingOperator segment.operator
        { state
            | argumentStack = state.argumentStack ++ List.map Number.toInt segment.arguments
            , length = List.length state.argumentStack + List.length segment.arguments
        }


{-| Is the current byte (the start of) a number?
-}
isNumberByte : Int -> Bool
isNumberByte byte =
    byte == 28 || byte >= 32


type alias State =
    { argumentStack : List Int
    , numberOfStems : Int
    , hstem : Int
    , vstem : Int
    , point : Point
    , length : Int
    , local : Maybe Subroutines
    , global : Subroutines
    , width : Maybe Int
    }


initialState : State
initialState =
    { argumentStack = []
    , numberOfStems = 0
    , hstem = 0
    , vstem = 0
    , point = { x = 0, y = 0 }
    , length = 0
    , local = Nothing
    , global = Array.empty
    , width = Nothing
    }


call : Subroutines -> Int -> State -> Maybe (EndReached ( List Operation, State ))
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
                    decodeSubroutine state
            in
            case Decode.decode decoder buffer of
                Nothing ->
                    -- ERROR error in local subroutine decoding
                    Nothing

                Just (EndReached ( operations, newState )) ->
                    Just (EndReached ( List.concat operations, newState ))

                Just (EndNotReached ( operations, newState )) ->
                    Just (EndNotReached ( List.concat operations, newState ))


mask : Int -> (List Int -> Operation) -> State -> Decoder ( List Operation, State )
mask op toOperation state =
    Decode.Extra.unfold (maskStep op toOperation) state


maskStep : Int -> (List Int -> Operation) -> State -> Decoder (Step ( Operation, State ) ( Operation, State ))
maskStep op toOperation state =
    if not (List.isEmpty state.argumentStack) then
        -- handle implicit `vstem` operations
        case state.argumentStack of
            dx :: dStem :: rest ->
                let
                    x =
                        state.vstem + dx
                in
                ( VStem x (x + dStem)
                , { state
                    | numberOfStems = 1 + state.numberOfStems
                    , vstem = x + dStem
                    , argumentStack = rest
                  }
                )
                    |> Loop
                    |> Decode.succeed

            -- TODO are -20 and -21 special?
            _ ->
                Decode.Extra.failWith "invalid mask arguments "

    else
        let
            numBytes =
                (state.numberOfStems + 7) // 8
        in
        Decode.Extra.exactly numBytes Decode.unsignedInt8
            |> Decode.map
                (\bytes ->
                    Done ( toOperation bytes, state )
                )


hstem : State -> Decoder (Step ( Operation, State ) ( Operation, State ))
hstem state =
    if odd (List.length state.argumentStack) then
        case state.argumentStack of
            width :: rest ->
                Decode.succeed <| Loop ( Width width, { state | argumentStack = rest } )

            _ ->
                Decode.Extra.failWith "hstem: invalid number of arguments for width"

    else
        case state.argumentStack of
            y :: dy :: rest ->
                let
                    newY =
                        state.hstem + y
                in
                Decode.succeed <|
                    loopIf (rest /= [])
                        ( HStem newY (newY + dy)
                        , { state
                            | numberOfStems = state.numberOfStems + 1
                            , hstem = newY + dy
                            , argumentStack = rest
                          }
                        )

            _ ->
                Decode.Extra.failWith "hstem: invalid number of arguments"


vstem : State -> Decoder (Step ( Operation, State ) ( Operation, State ))
vstem state =
    if odd (List.length state.argumentStack) then
        case state.argumentStack of
            width :: rest ->
                Decode.succeed (Loop ( Width width, { state | argumentStack = rest } ))

            _ ->
                Decode.Extra.failWith "vstem: invalid number of arguments for width"

    else
        case state.argumentStack of
            x :: dx :: rest ->
                let
                    newX =
                        state.vstem + x
                in
                Decode.succeed <|
                    loopIf (rest /= [])
                        ( VStem newX (newX + dx)
                        , { state
                            | numberOfStems = state.numberOfStems + 1
                            , vstem = newX + dx
                            , argumentStack = rest
                          }
                        )

            _ ->
                Decode.Extra.failWith "vstem: invalid number of arguments"


moveto state ( dx, dy ) newNumbers =
    let
        newPoint =
            { x = state.point.x + dx, y = state.point.y + dy }
    in
    Decode.succeed <|
        Done
            ( MoveTo newPoint
            , { state
                | point = newPoint
                , argumentStack = newNumbers
              }
            )


movetoWidth state op =
    case state.argumentStack of
        width :: rest ->
            Decode.succeed <|
                Loop ( Width width, { state | argumentStack = rest } )

        _ ->
            Decode.Extra.failWith "moveto width: invalid number of arguments"


vmoveto : State -> Decoder (Step ( Operation, State ) ( Operation, State ))
vmoveto state =
    if List.length state.argumentStack == 2 then
        movetoWidth state 4

    else
        case state.argumentStack of
            dy :: rest ->
                moveto state ( 0, dy ) rest

            _ ->
                Decode.Extra.failWith "vmoveto: invalid number of arguments"


hmoveto : State -> Decoder (Step ( Operation, State ) ( Operation, State ))
hmoveto state =
    if List.length state.argumentStack == 2 then
        movetoWidth state 22

    else
        case state.argumentStack of
            dx :: rest ->
                moveto state ( dx, 0 ) rest

            _ ->
                Decode.Extra.failWith "hmoveto: invalid number of arguments"


rmoveto : State -> Decoder (Step ( Operation, State ) ( Operation, State ))
rmoveto state =
    if List.length state.argumentStack == 3 then
        movetoWidth state 21

    else
        case state.argumentStack of
            dx :: dy :: rest ->
                moveto state ( dx, dy ) rest

            _ ->
                Decode.Extra.failWith "rmoveto: invalid number of arguments"


lineto : Int -> State -> ( Int, Int ) -> List Int -> ( Operation, State )
lineto op state ( dx, dy ) newNumbers =
    let
        newPoint =
            { x = state.point.x + dx
            , y = state.point.y + dy
            }
    in
    ( LineTo newPoint
    , { state
        | argumentStack = newNumbers
        , point = newPoint
      }
    )


rlineto : Int -> State -> Decoder (Step ( Operation, State ) ( Operation, State ))
rlineto op state =
    case state.argumentStack of
        dx :: dy :: rest ->
            Decode.succeed <| loopIf (rest /= []) (lineto op state ( dx, dy ) rest)

        _ ->
            Decode.Extra.failWith "rlineto: invalid number of arguments"


linetoX op state =
    case state.argumentStack of
        dx :: rest ->
            Decode.succeed <| loopIf (rest /= []) (lineto op state ( dx, 0 ) rest)

        _ ->
            Decode.Extra.failWith "linetoX: invalid number of arguments"


linetoY op state =
    case state.argumentStack of
        dy :: rest ->
            Decode.succeed <| loopIf (rest /= []) (lineto op state ( 0, dy ) rest)

        _ ->
            Decode.Extra.failWith "linetoX: invalid number of arguments"


hlineto : State -> Decoder (Step ( Operation, State ) ( Operation, State ))
hlineto state =
    if odd state.length == odd (List.length state.argumentStack) then
        linetoX 6 state

    else
        linetoY 6 state


vlineto : State -> Decoder (Step ( Operation, State ) ( Operation, State ))
vlineto state =
    if odd state.length == odd (List.length state.argumentStack) then
        linetoY 7 state

    else
        linetoX 7 state


{-| Call a local subroutine
-}
callsubr : State -> Maybe (EndReached ( List Operation, State ))
callsubr state =
    unSnoc state.argumentStack
        |> Maybe.andThen
            (\( index, previous ) ->
                case state.local of
                    Nothing ->
                        Nothing

                    Just subroutines ->
                        call subroutines index { state | argumentStack = previous }
            )


{-| Call a global subroutine
-}
callgsubr : State -> Maybe (EndReached ( List Operation, State ))
callgsubr state =
    case unSnoc state.argumentStack of
        Just ( index, newNumbers ) ->
            call state.global index { state | argumentStack = newNumbers }

        Nothing ->
            Nothing


rcurveline state =
    if List.length state.argumentStack == 2 then
        rlineto 24 state

    else
        rrcurveto 24 state


rlinecurve state =
    if List.length state.argumentStack == 6 then
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
    ( newPoint, CurveTo a b newPoint )


curvetoHelper op state rest ( newPoint, operation ) =
    Decode.succeed <|
        loopIf (rest /= [])
            ( operation
            , { state
                | point = newPoint
                , argumentStack = rest
              }
            )


loopIf condition =
    if condition then
        Loop

    else
        Done


rrcurveto : Int -> State -> Decoder (Step ( Operation, State ) ( Operation, State ))
rrcurveto op state =
    case state.argumentStack of
        dx :: dy :: dx2 :: dy2 :: dx3 :: dy3 :: rest ->
            curveto state.point dx dy dx2 dy2 dx3 dy3
                |> curvetoHelper 8 state rest

        _ ->
            Decode.Extra.failWith "rrcurveto: invalid number of arguments"


vvcurveto state =
    if odd (List.length state.argumentStack) then
        case state.argumentStack of
            dx :: dy :: dx2 :: dy2 :: d :: rest ->
                curveto state.point dx dy dx2 dy2 0 d
                    |> curvetoHelper 26 state rest

            _ ->
                Decode.Extra.failWith "vvcurveto: invalid number of arguments in the odd case"

    else
        case state.argumentStack of
            dy :: dx2 :: dy2 :: d :: rest ->
                curveto state.point 0 dy dx2 dy2 0 d
                    |> curvetoHelper 26 state rest

            _ ->
                Decode.Extra.failWith "vvcurveto: invalid number of arguments in the even case"


hhcurveto state =
    if odd (List.length state.argumentStack) then
        case state.argumentStack of
            dy :: dx :: dx2 :: dy2 :: d :: rest ->
                curveto state.point dx dy dx2 dy2 d 0
                    |> curvetoHelper 27 state rest

            _ ->
                Decode.Extra.failWith "hhcurveto: invalid number of arguments in the odd case"

    else
        case state.argumentStack of
            dx :: dx2 :: dy2 :: d :: rest ->
                curveto state.point dx 0 dx2 dy2 d 0
                    |> curvetoHelper 27 state rest

            _ ->
                Decode.Extra.failWith "hhcurveto: invalid number of arguments in the even case"


vhcurveto : State -> Decoder ( List Operation, State )
vhcurveto state =
    let
        ( _, cursor, curves ) =
            vhGeneralCase state
                |> vhSpecialCase1 state
                |> vhSpecialCase2 state
    in
    Decode.succeed
        ( List.reverse curves
        , { state | argumentStack = [], point = cursor }
        )


hvcurveto : State -> Decoder ( List Operation, State )
hvcurveto state =
    let
        ( _, cursor, curves ) =
            hvGeneralCase state
                |> hvSpecialCase1 state
                |> hvSpecialCase2 state
    in
    Decode.succeed
        ( List.reverse curves
        , { state | argumentStack = [], point = cursor }
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
    looper 0 state.point state.argumentStack []


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
    looper 0 state.point state.argumentStack []


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


decodeDrawingOperator : Int -> State -> Decoder ( List Operation, State )
decodeDrawingOperator code state =
    case code of
        1 ->
            Decode.Extra.unfold hstem state

        18 ->
            Decode.Extra.unfold hstem state

        3 ->
            Decode.Extra.unfold vstem state

        23 ->
            Decode.Extra.unfold vstem state

        4 ->
            Decode.Extra.unfold vmoveto state

        5 ->
            Decode.Extra.unfold (rlineto 5) state

        6 ->
            Decode.Extra.unfold hlineto state

        7 ->
            Decode.Extra.unfold vlineto state

        8 ->
            Decode.Extra.unfold (rrcurveto 8) state

        -- 11, 14, 19 and 20, 10, 29 are all handled elsewhere
        21 ->
            Decode.Extra.unfold rmoveto state

        22 ->
            Decode.Extra.unfold hmoveto state

        24 ->
            Decode.Extra.unfold rcurveline state

        25 ->
            Decode.Extra.unfold rlinecurve state

        26 ->
            Decode.Extra.unfold vvcurveto state

        27 ->
            Decode.Extra.unfold hhcurveto state

        30 ->
            vhcurveto state

        31 ->
            hvcurveto state

        _ ->
            Decode.Extra.failWith ("decodeDrawingOperator: unkown charstring operator: " ++ String.fromInt code)


even : Int -> Bool
even x =
    (x |> modBy 2) == 0


odd : Int -> Bool
odd x =
    (x |> modBy 2) /= 0


{-| pop the last element off a list
-}
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
