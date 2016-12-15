module View exposing (..)

import Formatting as F exposing ((<>))
import Html as Html exposing (Html)
import Html.Attributes as Html
import Html.Events as Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time)
import Types exposing (..)


root : Model -> Html Msg
root model =
    Html.div []
        [ Html.button
            [ Html.style
                [ ( "position", "absolute" )
                , ( "top", "5px" )
                , ( "left", "5px" )
                , ( "border", "solid 1px black" )
                , ( "border-radius", "0" )
                , ( "background-color", "white" )
                , ( "font-size", "18px" )
                ]
            , Html.onClick Inc
            ]
            [ Html.text "Next..." ]
        , svg
            [ width "100vw"
            , height "100vh"
            , viewBox
                (F.print (F.int <> F.s " " <> F.int <> F.s " " <> F.int <> F.s " " <> F.int)
                    (model.frame.x - 1)
                    (model.frame.y - 1)
                    (model.frame.width + 2)
                    (model.frame.height + 2)
                )
            ]
            [ polyline
                [ stroke "black"
                , fill "none"
                , strokeWidth "0.3"
                , strokeLinecap "round"
                , points
                    ((toString (model.frame.x + 1) ++ "," ++ toString model.frame.y)
                        ++ " "
                        ++ (toString (model.frame.x + model.frame.width) ++ "," ++ toString model.frame.y)
                        ++ " "
                        ++ (toString (model.frame.x + model.frame.width) ++ "," ++ toString (model.frame.y + model.frame.height))
                    )
                ]
                []
            , polyline
                [ stroke "black"
                , fill "none"
                , strokeWidth "0.3"
                , strokeLinecap "round"
                , points
                    ((toString (model.frame.x + model.frame.width - 1) ++ "," ++ toString (model.frame.y + model.frame.height))
                        ++ " "
                        ++ (toString (model.frame.x) ++ "," ++ toString (model.frame.y + model.frame.height))
                        ++ " "
                        ++ (toString (model.frame.x) ++ "," ++ toString model.frame.y)
                    )
                ]
                []
            , g [] (List.map (drawPartition model.elapsed) model.maze)
            ]
        ]


drawPartition : Time -> Partition -> Svg msg
drawPartition elapsed partition =
    g []
        (case partition.direction of
            Horizontal ->
                [ drawWall elapsed
                    partition.depth
                    (partition.a.start)
                    (partition.a.start + partition.a.length)
                    (partition.cross)
                    (partition.cross)
                , drawWall elapsed
                    partition.depth
                    (partition.b.start + partition.b.length)
                    (partition.b.start)
                    (partition.cross)
                    (partition.cross)
                ]

            Vertical ->
                [ drawWall elapsed
                    partition.depth
                    (partition.cross)
                    (partition.cross)
                    (partition.a.start)
                    (partition.a.start + partition.a.length)
                , drawWall elapsed
                    partition.depth
                    (partition.cross)
                    (partition.cross)
                    (partition.b.start + partition.b.length)
                    (partition.b.start)
                ]
        )


drawWall : Time -> Int -> Int -> Int -> Int -> Int -> Svg msg
drawWall elapsed depth px1 px2 py1 py2 =
    let
        percentage =
            ((elapsed * 4 / Time.second) - (toFloat depth))
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
                , x2 <| toString (toFloat px1 + ((toFloat (px2 - px1)) * percentage))
                , y2 <| toString (toFloat py1 + ((toFloat (py2 - py1)) * percentage))
                ]
                []
