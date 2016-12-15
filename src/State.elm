module State exposing (..)

import AnimationFrame
import Random exposing (..)
import Types exposing (..)


init : ( Model, Cmd Msg )
init =
    ( { frame =
            { x = 0
            , y = 0
            , width = 50
            , height = 50
            }
      , seed = initialSeed 1
      , elapsed = 0
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeMaze ->
            ( { model
                | elapsed = 0
                , seed = Tuple.second <| step bool model.seed
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
