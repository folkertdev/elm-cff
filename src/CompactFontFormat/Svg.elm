module CompactFontFormat.Svg exposing (glyph, convert)

{-| Convert the CFF drawing instructions into SVG drawing instructions

Not all CFF instructions are relevant for svg drawing. For instance the hints are not used
because these are signals to the rasterizer. With SVG, the browser rasterizer will take care of the rasterization automatically.
It cannot be influenced from SVG path instructions.

The functions in this module will automatically close the path.


## Functions

@docs glyph, convert

-}

import Charstring exposing (Charstring, Operation(..))
import Path.LowLevel as Path exposing (DrawTo, Mode(..), MoveTo, SubPath)


{-| Convert a list of drawing operations into SVG path syntax instructions

The resulting string can be plugged into an svg `d` attribute.

-}
glyph : List Operation -> String
glyph operations =
    Path.toString (convert operations)


{-| Convert a list of drawing operations into SVG path syntax instructions
-}
convert : List Operation -> List SubPath
convert operations =
    case operations of
        (MoveTo point) :: rest ->
            convertHelp rest (Path.MoveTo Absolute (asFloats ( point.x, point.y ))) [] []

        _ :: rest ->
            convert rest

        [] ->
            Debug.log "operation skipped in convert" []


asFloats ( x, y ) =
    ( toFloat x, toFloat y )


convertHelp : List Operation -> MoveTo -> List DrawTo -> List SubPath -> List SubPath
convertHelp operations moveto drawtos subpaths =
    case operations of
        [] ->
            let
                last =
                    { moveto = moveto, drawtos = List.reverse (Path.ClosePath :: drawtos) }
            in
            List.reverse (last :: subpaths)

        first :: rest ->
            case first of
                MoveTo point ->
                    convertHelp rest (Path.MoveTo Absolute (asFloats ( point.x, point.y ))) [] ({ moveto = moveto, drawtos = List.reverse (Path.ClosePath :: drawtos) } :: subpaths)

                CurveTo p1 p2 p3 ->
                    let
                        argument =
                            ( asFloats ( p1.x, p1.y ), asFloats ( p2.x, p2.y ), asFloats ( p3.x, p3.y ) )
                    in
                    convertHelp rest moveto (Path.CurveTo Absolute [ argument ] :: drawtos) subpaths

                LineTo p ->
                    convertHelp rest moveto (Path.LineTo Absolute [ asFloats ( p.x, p.y ) ] :: drawtos) subpaths

                CounterMask _ ->
                    convertHelp rest moveto drawtos subpaths

                HintMask _ ->
                    convertHelp rest moveto drawtos subpaths

                HStem y dy ->
                    convertHelp rest moveto drawtos subpaths

                VStem x dx ->
                    convertHelp rest moveto drawtos subpaths

                Width w ->
                    convertHelp rest moveto drawtos subpaths
