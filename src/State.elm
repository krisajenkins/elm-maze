module State exposing (..)

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
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeMaze ->
            ( { model | seed = Tuple.second <| step bool model.seed }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
