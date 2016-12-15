module Types exposing (..)

import Random exposing (..)
import Time exposing (..)


type Msg
    = ChangeMaze
    | Tick Time


type alias Model =
    { frame : Box
    , seed : Seed
    , elapsed : Time
    }


type Direction
    = Horizontal
    | Vertical


rotate : Direction -> Direction
rotate direction =
    case direction of
        Vertical ->
            Horizontal

        Horizontal ->
            Vertical


type alias Partition =
    { depth : Int
    , direction : Direction
    , crossAt : Int
    , a : { start : Int, length : Int }
    , b : { start : Int, length : Int }
    }


type alias Box =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    }


reducer :
    (Box -> Seed -> ( List Partition, Seed ))
    -> Box
    -> ( List Partition, Seed )
    -> ( List Partition, Seed )
reducer splitter subBox ( partitions, seed ) =
    let
        ( subPartitions, newSeed ) =
            splitter subBox seed
    in
        ( List.concat [ partitions, subPartitions ]
        , newSeed
        )


splitBox : Direction -> Int -> Box -> Seed -> ( List Partition, Seed )
splitBox direction depth box seed =
    if direction == Horizontal && box.width <= 1 then
        ( [], seed )
    else if direction == Vertical && box.height <= 1 then
        ( [], seed )
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
            List.foldl (reducer (splitBox (rotate direction) (depth + 1)))
                ( [ case direction of
                        Horizontal ->
                            { depth = depth
                            , direction = direction
                            , crossAt = y2
                            , a = { start = x1, length = xSplit - 1 }
                            , b = { start = x2, length = box.width - xSplit }
                            }

                        Vertical ->
                            { depth = depth
                            , direction = direction
                            , crossAt = x2
                            , a = { start = y1, length = ySplit - 1 }
                            , b = { start = y2, length = box.height - ySplit }
                            }
                  ]
                , newSeed
                )
                (case direction of
                    Horizontal ->
                        [ { x = x1
                          , y = y1
                          , width = box.width
                          , height = ySplit
                          }
                        , { x = x1
                          , y = y2
                          , width = box.width
                          , height = box.height - ySplit
                          }
                        ]

                    Vertical ->
                        [ { x = x1
                          , y = y1
                          , width = xSplit
                          , height = box.height
                          }
                        , { x = x2
                          , y = y1
                          , width = box.width - xSplit
                          , height = box.height
                          }
                        ]
                )
