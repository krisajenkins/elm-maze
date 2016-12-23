module Maze exposing (generate)

import Random exposing (..)
import Types exposing (..)


generate : Box -> Seed -> ( List Partition, Seed )
generate =
    generateStep Horizontal 0


generateStep :
    Direction
    -> Int
    -> Box
    -> Seed
    -> ( List Partition, Seed )
generateStep direction depth box seed =
    case splitBox direction depth box seed of
        Nothing ->
            ( [], seed )

        Just ( ( partition, subBoxA, subBoxB ), newSeed ) ->
            let
                substep =
                    generateStep (rotate direction) (depth + 1)

                ( subPartitionsA, seedA ) =
                    substep subBoxA newSeed

                ( subPartitionsB, seedB ) =
                    substep subBoxB seedA
            in
                ( List.concat
                    [ [ partition ]
                    , subPartitionsA
                    , subPartitionsB
                    ]
                , seedB
                )


splitBox :
    Direction
    -> Int
    -> Box
    -> Seed
    -> Maybe ( ( Partition, Box, Box ), Seed )
splitBox direction depth box seed =
    if direction == Horizontal && box.width <= 1 then
        Nothing
    else if direction == Vertical && box.height <= 1 then
        Nothing
    else
        let
            ( ( xSplit, ySplit ), newSeed ) =
                Random.step
                    (pair (int 1 (box.width - 1))
                        (int 1 (box.height - 1))
                    )
                    seed

            x1 =
                box.x

            x2 =
                box.x + xSplit

            y1 =
                box.y

            y2 =
                box.y + ySplit
        in
            Just
                ( case direction of
                    Horizontal ->
                        ( { depth = depth
                          , direction = direction
                          , crossAt = y2
                          , a = { start = x1, length = xSplit - 1 }
                          , b = { start = x2, length = box.width - xSplit }
                          }
                        , Box x1 y1 box.width ySplit
                        , Box x1 y2 box.width (box.height - ySplit)
                        )

                    Vertical ->
                        ( { depth = depth
                          , direction = direction
                          , crossAt = x2
                          , a = { start = y1, length = ySplit - 1 }
                          , b = { start = y2, length = box.height - ySplit }
                          }
                        , Box x1 y1 xSplit box.height
                        , Box x2 y1 (box.width - xSplit) box.height
                        )
                , newSeed
                )
