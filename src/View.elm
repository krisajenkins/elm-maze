module View exposing (..)

import Formatting as F exposing ((<>), Format(..), s)
import Html as Html exposing (Html)
import Html.Events as Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time)
import Types exposing (..)


pair : Format r (( Int, Int ) -> r)
pair =
    Format
        (\c ( x, y ) ->
            c
                <| F.print (F.int <> s "," <> F.int)
                    x
                    y
        )


root : Model -> Html Msg
root model =
    Html.div []
        [ svg
            [ width "100vw"
            , height "100vh"
            , viewBox
                (F.print (F.int <> s " " <> F.int <> s " " <> F.int <> s " " <> F.int)
                    (model.frame.x - 1)
                    (model.frame.y - 1)
                    (model.frame.width + 2)
                    (model.frame.height + 2)
                )
            , Html.onClick ChangeMaze
            ]
            [ border model.frame
            , g []
                <| List.map (drawPartition model.elapsed)
                <| Tuple.first
                <| splitBox Horizontal 0 model.frame model.seed
            ]
        ]


border : Box -> Svg msg
border frame =
    g []
        [ polyline
            [ stroke "black"
            , fill "none"
            , strokeWidth "0.3"
            , strokeLinecap "round"
            , points
                (F.print (pair <> s " " <> pair <> s " " <> pair)
                    ( frame.x + 1, frame.y )
                    ( frame.x + frame.width, frame.y )
                    ( frame.x + frame.width, frame.y + frame.height )
                )
            ]
            []
        , polyline
            [ stroke "black"
            , fill "none"
            , strokeWidth "0.3"
            , strokeLinecap "round"
            , points
                (F.print (pair <> s " " <> pair <> s " " <> pair)
                    ( frame.x + frame.width - 1, frame.y + frame.height )
                    ( frame.x, frame.y + frame.height )
                    ( frame.x, frame.y )
                )
            ]
            []
        ]


drawPartition : Time -> Partition -> Svg msg
drawPartition elapsed partition =
    case partition.direction of
        Horizontal ->
            g []
                [ drawWall elapsed
                    partition.depth
                    (partition.a.start)
                    (partition.a.start + partition.a.length)
                    (partition.crossAt)
                    (partition.crossAt)
                , drawWall elapsed
                    partition.depth
                    (partition.b.start + partition.b.length)
                    (partition.b.start)
                    (partition.crossAt)
                    (partition.crossAt)
                ]

        Vertical ->
            g []
                [ drawWall elapsed
                    partition.depth
                    (partition.crossAt)
                    (partition.crossAt)
                    (partition.a.start)
                    (partition.a.start + partition.a.length)
                , drawWall elapsed
                    partition.depth
                    (partition.crossAt)
                    (partition.crossAt)
                    (partition.b.start + partition.b.length)
                    (partition.b.start)
                ]


drawWall : Time -> Int -> Int -> Int -> Int -> Int -> Svg msg
drawWall elapsed depth px1 px2 py1 py2 =
    let
        percentage =
            ((elapsed * 3 / Time.second) - (toFloat depth))
                |> Basics.min 1
                |> Basics.max 0
    in
        if percentage == 0 then
            g [] []
        else
            line
                [ stroke "black"
                , strokeLinecap "round"
                , strokeWidth "0.3"
                , x1 <| toString px1
                , y1 <| toString py1
                , x2 <| toString (toFloat px1 + (toFloat (px2 - px1) * percentage))
                , y2 <| toString (toFloat py1 + (toFloat (py2 - py1) * percentage))
                ]
                []
