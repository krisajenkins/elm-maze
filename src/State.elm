module State exposing (..)

import AnimationFrame
import Random exposing (..)
import Types exposing (..)


foo :
    (Box -> Seed -> ( List Partition, Seed ))
    -> Box
    -> ( List Partition, Seed )
    -> ( List Partition, Seed )
foo splitter subBox ( partitions, seed ) =
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
            List.foldl (foo (splitBox (rotate direction) (depth + 1)))
                ( [ case direction of
                        Horizontal ->
                            { depth = depth + 1
                            , direction = direction
                            , cross = y2
                            , a = { start = x1, length = x2 - x1 - 1 }
                            , b = { start = x2, length = box.width - xSplit }
                            }

                        Vertical ->
                            { depth = depth + 1
                            , direction = direction
                            , cross = x2
                            , a = { start = y1, length = y2 - y1 - 1 }
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


init : ( Model, Cmd Msg )
init =
    let
        frame =
            { x = 0
            , y = 0
            , width = 60
            , height = 40
            }

        -- TODO
        seed =
            initialSeed counter

        counter =
            1
    in
        ( { frame = frame
          , maze =
                splitBox Vertical 0 frame seed
                    |> Tuple.first
          , seed = seed
          , counter = counter
          , elapsed = 0
          }
        , Cmd.none
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Inc ->
            let
                newCounter =
                    model.counter + 1

                newSeed =
                    initialSeed newCounter
            in
                ( { model
                    | counter = newCounter
                    , seed = newSeed
                    , maze =
                        splitBox Vertical 0 model.frame newSeed
                            |> Tuple.first
                    , elapsed = 0
                  }
                , Cmd.none
                )

        Tick t ->
            ( { model | elapsed = model.elapsed + t }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    AnimationFrame.diffs Tick
