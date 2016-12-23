module Types exposing (..)

import Random exposing (..)
import Window


type Msg
    = ChangeMaze
    | Resize Window.Size


type alias Model =
    { frame : Maybe Box
    , seed : Seed
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
