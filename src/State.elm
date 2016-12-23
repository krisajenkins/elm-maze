module State exposing (..)

import Random exposing (..)
import Task
import Types exposing (..)
import Window


init : ( Model, Cmd Msg )
init =
    ( { frame = Nothing
      , seed = initialSeed 1
      }
    , Task.perform Resize Window.size
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize size ->
            let
                mazeSimplicity =
                    20
            in
                ( { model
                    | frame =
                        Just
                            { x = 0
                            , y = 0
                            , width = size.width // mazeSimplicity
                            , height = size.height // mazeSimplicity
                            }
                    , seed = initialSeed <| size.width + size.height
                  }
                , Cmd.none
                )

        ChangeMaze ->
            ( { model | seed = Tuple.second <| step bool model.seed }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Window.resizes Resize
