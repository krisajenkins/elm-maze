module Types exposing (..)

import Random exposing (Seed)
import Time exposing (..)


type Msg
    = Inc
    | Tick Time


type alias Box =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
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
    , cross : Int
    , a : { start : Int, length : Int }
    , b : { start : Int, length : Int }
    }


type alias Model =
    { frame : Box
    , maze : List Partition
    , seed : Seed
    , counter : Int
    , elapsed : Time
    }
