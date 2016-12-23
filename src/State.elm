module State exposing (..)

import Random exposing (..)
import Task
import Time
import Time exposing (Time)
import Types exposing (..)
import Window


init : ( Model, Cmd Msg )
init =
    ( { frame = Nothing
      , seed = Nothing
      }
    , Cmd.batch
        [ Task.perform Resize Window.size
        , Task.perform StartupTime Time.now
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize size ->
            let
                mazeSimplicity =
                    30
            in
                ( { model
                    | frame =
                        Just
                            { x = 0
                            , y = 0
                            , width = size.width // mazeSimplicity
                            , height = size.height // mazeSimplicity
                            }
                  }
                , Cmd.none
                )

        StartupTime t ->
            ( { model | seed = Just <| initialSeed <| round t }
            , Cmd.none
            )

        ChangeMaze ->
            ( { model | seed = Maybe.map (Tuple.second << step bool) model.seed }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Window.resizes Resize
