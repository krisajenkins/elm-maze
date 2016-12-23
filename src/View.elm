module View exposing (..)

import Formatting as F exposing ((<>), Format(..), s)
import Html as Html exposing (Html)
import Html.Attributes as Html
import Html.Events as Html
import Html.Keyed
import Maze
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Lazy as Svg
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
    case ( model.frame, model.seed ) of
        ( Just frame, Just seed ) ->
            Html.Keyed.node "div"
                []
                [ ( toString seed
                  , svg
                        [ width "100vw"
                        , height "100vh"
                        , viewBox
                            (F.print (F.int <> s " " <> F.int <> s " " <> F.int <> s " " <> F.int)
                                (frame.x - 1)
                                (frame.y - 1)
                                (frame.width + 2)
                                (frame.height + 2)
                            )
                        , Html.onClick ChangeMaze
                        ]
                        [ border frame
                        , g []
                            <| List.map (Svg.lazy drawPartition)
                            <| Tuple.first
                            <| Maze.generate frame seed
                        ]
                  )
                ]

        _ ->
            text ""


heading : Html msg
heading =
    Html.h1 [ Html.style [] ]
        [ Html.text "Click to regenerate" ]


border : Box -> Svg msg
border frame =
    g []
        [ polyline
            [ stroke "black"
            , fill "none"
            , strokeWidth "0.3"
            , strokeLinecap "square"
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
            , strokeLinecap "square"
            , points
                (F.print (pair <> s " " <> pair <> s " " <> pair)
                    ( frame.x + frame.width - 1, frame.y + frame.height )
                    ( frame.x, frame.y + frame.height )
                    ( frame.x, frame.y )
                )
            ]
            []
        ]


drawPartition : Partition -> Svg msg
drawPartition partition =
    case partition.direction of
        Horizontal ->
            g []
                [ drawWall partition.depth
                    (partition.a.start)
                    (partition.a.start + partition.a.length)
                    (partition.crossAt)
                    (partition.crossAt)
                , drawWall partition.depth
                    (partition.b.start + partition.b.length)
                    (partition.b.start)
                    (partition.crossAt)
                    (partition.crossAt)
                ]

        Vertical ->
            g []
                [ drawWall partition.depth
                    (partition.crossAt)
                    (partition.crossAt)
                    (partition.a.start)
                    (partition.a.start + partition.a.length)
                , drawWall partition.depth
                    (partition.crossAt)
                    (partition.crossAt)
                    (partition.b.start + partition.b.length)
                    (partition.b.start)
                ]


drawWall : Int -> Int -> Int -> Int -> Int -> Svg msg
drawWall depth px1 px2 py1 py2 =
    let
        lineTime =
            500

        beginAt =
            F.print (F.int <> s "ms") (depth * lineTime)

        duration =
            F.print (F.int <> s "ms") lineTime
    in
        line
            [ stroke "black"
            , strokeLinecap "square"
            , strokeWidth "0.3"
            , x1 <| toString px1
            , y1 <| toString py1
            , x2 <| toString px1
            , y2 <| toString py1
            , opacity "0"
            ]
            [ animate
                [ attributeName "x2"
                , from <| toString px1
                , to <| toString px2
                , fill "freeze"
                , begin beginAt
                , dur duration
                ]
                []
            , animate
                [ attributeName "y2"
                , from <| toString py1
                , to <| toString py2
                , fill "freeze"
                , begin beginAt
                , dur duration
                ]
                []
            , animate
                [ attributeName "opacity"
                , from "0"
                , to "1"
                , fill "freeze"
                , begin beginAt
                , dur "1ms"
                ]
                []
            ]
