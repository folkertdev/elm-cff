module Encoding exposing (Encodings, decode, get)

import Array exposing (Array)
import Bytes.Decode as Decode exposing (Decoder)
import Utils


type Encodings
    = Expert
    | Format0 Format0Encoding
    | Format1 Format1Encoding
    | Standard


type alias Format0Encoding =
    Array Int


type alias Format1Encoding =
    Array Range


decode : Int -> Decoder Encodings
decode encoding =
    case encoding of
        0 ->
            Decode.succeed Standard

        1 ->
            Decode.succeed Expert

        _ ->
            Decode.unsignedInt8
                |> Decode.andThen
                    (\format ->
                        case format of
                            0 ->
                                Decode.map Format0 decodeFormat0

                            1 ->
                                Decode.map Format1 decodeFormat1

                            _ ->
                                Decode.fail
                    )


decodeFormat0 =
    Decode.unsignedInt8
        |> Decode.andThen
            (\num_codes ->
                Utils.exactly num_codes Decode.unsignedInt8
                    |> Decode.map Array.fromList
            )


decodeFormat1 =
    Decode.unsignedInt8
        |> Decode.andThen
            (\numRanges ->
                Utils.exactly numRanges decodeRange
                    |> Decode.map Array.fromList
            )


type alias Range =
    { first : Int, nLeft : Int }


decodeRange =
    Decode.map2 Range Decode.unsignedInt8 Decode.unsignedInt8



-- Get


{-| Returns the string ID for the supplied codepoint.
-}
get : Int -> Encodings -> Int
get codepoint encodings =
    case encodings of
        Format0 array ->
            getFormat0 codepoint array

        Format1 array ->
            getFormat1 codepoint array

        -- https://github.com/glyph-rs/cff/blob/master/src/glyphs/encodings.rs#L126
        Expert ->
            Debug.todo "expert"

        Standard ->
            Debug.todo "standart"


getFormat0 : Int -> Format0Encoding -> Int
getFormat0 codepoint array =
    case Array.get codepoint array of
        Just i ->
            -- add `1` because `.notdef`
            i + 1

        Nothing ->
            0


getFormat1 : Int -> Format1Encoding -> Int
getFormat1 codepoint_ array =
    let
        folder range accum =
            case accum of
                Ok answer ->
                    Ok answer

                Err codepoint ->
                    if codepoint < range.nLeft then
                        Ok (range.first + codepoint + 1)

                    else
                        Err (codepoint - (range.nLeft + 1))
    in
    Array.foldl folder (Err codepoint_) array
        |> Result.withDefault 0



-- Tables


expert =
    Array.fromList
        [ ( 32, 1 )
        , ( 33, 229 )
        , ( 34, 230 )
        , ( 36, 231 )
        , ( 37, 232 )
        , ( 38, 233 )
        , ( 39, 234 )
        , ( 40, 235 )
        , ( 41, 236 )
        , ( 42, 237 )
        , ( 43, 238 )
        , ( 44, 13 )
        , ( 45, 14 )
        , ( 46, 15 )
        , ( 47, 99 )
        , ( 48, 239 )
        , ( 49, 240 )
        , ( 50, 241 )
        , ( 51, 242 )
        , ( 52, 243 )
        , ( 53, 244 )
        , ( 54, 245 )
        , ( 55, 246 )
        , ( 56, 247 )
        , ( 57, 248 )
        , ( 58, 27 )
        , ( 59, 28 )
        , ( 60, 249 )
        , ( 61, 250 )
        , ( 62, 251 )
        , ( 63, 252 )
        , ( 65, 253 )
        , ( 66, 254 )
        , ( 67, 255 )
        , ( 68, 256 )
        , ( 69, 257 )
        , ( 73, 258 )
        , ( 76, 259 )
        , ( 77, 260 )
        , ( 78, 261 )
        , ( 79, 262 )
        , ( 82, 263 )
        , ( 83, 264 )
        , ( 84, 265 )
        , ( 86, 266 )
        , ( 87, 109 )
        , ( 88, 110 )
        , ( 89, 267 )
        , ( 90, 268 )
        , ( 91, 269 )
        , ( 93, 270 )
        , ( 94, 271 )
        , ( 95, 272 )
        , ( 96, 273 )
        , ( 97, 274 )
        , ( 98, 275 )
        , ( 99, 276 )
        , ( 100, 277 )
        , ( 101, 278 )
        , ( 102, 279 )
        , ( 103, 280 )
        , ( 104, 281 )
        , ( 105, 282 )
        , ( 106, 283 )
        , ( 107, 284 )
        , ( 108, 285 )
        , ( 109, 286 )
        , ( 110, 287 )
        , ( 111, 288 )
        , ( 112, 289 )
        , ( 113, 290 )
        , ( 114, 291 )
        , ( 115, 292 )
        , ( 116, 293 )
        , ( 117, 294 )
        , ( 118, 295 )
        , ( 119, 296 )
        , ( 120, 297 )
        , ( 121, 298 )
        , ( 122, 299 )
        , ( 123, 300 )
        , ( 124, 301 )
        , ( 125, 302 )
        , ( 126, 303 )
        , ( 161, 304 )
        , ( 162, 305 )
        , ( 163, 306 )
        , ( 166, 307 )
        , ( 167, 308 )
        , ( 168, 309 )
        , ( 169, 310 )
        , ( 170, 311 )
        , ( 172, 312 )
        , ( 175, 313 )
        , ( 178, 314 )
        , ( 179, 315 )
        , ( 182, 316 )
        , ( 183, 317 )
        , ( 184, 318 )
        , ( 188, 158 )
        , ( 189, 155 )
        , ( 190, 163 )
        , ( 191, 319 )
        , ( 192, 320 )
        , ( 193, 321 )
        , ( 194, 322 )
        , ( 195, 323 )
        , ( 196, 324 )
        , ( 197, 325 )
        , ( 200, 326 )
        , ( 201, 150 )
        , ( 202, 164 )
        , ( 203, 169 )
        , ( 204, 327 )
        , ( 205, 328 )
        , ( 206, 329 )
        , ( 207, 330 )
        , ( 208, 331 )
        , ( 209, 332 )
        , ( 210, 333 )
        , ( 211, 334 )
        , ( 212, 335 )
        , ( 213, 336 )
        , ( 214, 337 )
        , ( 215, 338 )
        , ( 216, 339 )
        , ( 217, 340 )
        , ( 218, 341 )
        , ( 219, 342 )
        , ( 220, 343 )
        , ( 221, 344 )
        , ( 222, 345 )
        , ( 223, 346 )
        , ( 224, 347 )
        , ( 225, 348 )
        , ( 226, 349 )
        , ( 227, 350 )
        , ( 228, 351 )
        , ( 229, 352 )
        , ( 230, 353 )
        , ( 231, 354 )
        , ( 232, 355 )
        , ( 233, 356 )
        , ( 234, 357 )
        , ( 235, 358 )
        , ( 236, 359 )
        , ( 237, 360 )
        , ( 238, 361 )
        , ( 239, 362 )
        , ( 240, 363 )
        , ( 241, 364 )
        , ( 242, 365 )
        , ( 243, 366 )
        , ( 244, 367 )
        , ( 245, 368 )
        , ( 246, 369 )
        , ( 247, 370 )
        , ( 248, 371 )
        , ( 249, 372 )
        , ( 250, 373 )
        , ( 251, 374 )
        , ( 252, 375 )
        , ( 253, 376 )
        , ( 254, 377 )
        , ( 255, 378 )
        ]


standard =
    Array.fromList
        [ 32
        , 33
        , 34
        , 35
        , 36
        , 37
        , 38
        , 39
        , 40
        , 41
        , 42
        , 43
        , 44
        , 45
        , 46
        , 47
        , 48
        , 49
        , 50
        , 51
        , 52
        , 53
        , 54
        , 55
        , 56
        , 57
        , 58
        , 59
        , 60
        , 61
        , 62
        , 63
        , 64
        , 65
        , 66
        , 67
        , 68
        , 69
        , 70
        , 71
        , 72
        , 73
        , 74
        , 75
        , 76
        , 77
        , 78
        , 79
        , 80
        , 81
        , 82
        , 83
        , 84
        , 85
        , 86
        , 87
        , 88
        , 89
        , 90
        , 91
        , 92
        , 93
        , 94
        , 95
        , 96
        , 97
        , 98
        , 99
        , 100
        , 101
        , 102
        , 103
        , 104
        , 105
        , 106
        , 107
        , 108
        , 109
        , 110
        , 111
        , 112
        , 113
        , 114
        , 115
        , 116
        , 117
        , 118
        , 119
        , 120
        , 121
        , 122
        , 123
        , 124
        , 125
        , 126
        , 161
        , 162
        , 163
        , 164
        , 165
        , 166
        , 167
        , 168
        , 169
        , 170
        , 171
        , 172
        , 173
        , 174
        , 175
        , 177
        , 178
        , 179
        , 180
        , 182
        , 183
        , 184
        , 185
        , 186
        , 187
        , 188
        , 189
        , 191
        , 193
        , 194
        , 195
        , 196
        , 197
        , 198
        , 199
        , 200
        , 202
        , 203
        , 205
        , 206
        , 207
        , 208
        , 225
        , 227
        , 232
        , 233
        , 234
        , 235
        , 241
        , 245
        , 248
        , 249
        , 250
        , 251
        ]
