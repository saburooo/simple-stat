module Chart exposing (..)

import TypedSvg
import Svg as Svg
import TypedSvg.Attributes exposing (viewBox, width, height)
import TypedSvg.Attributes exposing (points)
import TypedSvg.Attributes exposing (fill)
import TypedSvg.Types as Types

import Color

import Tuple
import TypedSvg.Attributes exposing (stroke)
import TypedSvg.Attributes exposing (strokeWidth)
import TypedSvg.Attributes exposing (transform)

import Utility
import TypedSvg.Attributes.InEm exposing (x1, x2, y1, y2)


-- グラフを作成するためにタイプを生成
type Graph
        = Xscale
        | Yscale


type GraphType
        = Line
        | Histogram


-- SVG


backColor: Color.Color -> Svg.Svg msg
backColor color =
    Svg.rect [ width (Types.percent 100), height (Types.percent 100), fill (Types.Paint color) ] []


listVisualizeArgOne: List Float -> Svg.Svg msg
listVisualizeArgOne floatList =
    let
        floatListMap = ( List.map (\x -> x ^ 2 * 50) <| floatList )
        floatRange = List.map (\x -> toFloat x * 50) (List.range 1 ( (List.length floatListMap) + 1 ))
        tupleFloatList = List.map2 Tuple.pair floatRange floatListMap
        -- bundary = ( Maybe.withDefault 0 ( List.maximum floatList ) - Maybe.withDefault 0 ( List.minimum floatList ) ) / toFloat (List.length floatList)
        -- starJess = Utility.starJes floatList
    in
        Svg.svg [ viewBox 0 0 500 300 ]
            [ backColor Color.lightBlue
            , TypedSvg.g [ transform [ Types.Translate 0 200 ] ]
                [ Svg.line [ x1 0, y1 0, x2 500, y2 0, strokeWidth (Types.px 3), stroke (Types.Paint Color.white) ] []
                , Svg.line [ x1 2.5, y1 400, x2 2.5, y2 -100, strokeWidth (Types.px 3), stroke (Types.Paint Color.white) ] []
                , Svg.polyline [ points tupleFloatList
                            , fill ( Types.PaintNone )
                            , stroke (Types.Paint Color.blue)
                            , TypedSvg.Attributes.style "transform: scale(1, -1)"
                            , strokeWidth (Types.px 6) ] [] 
                ]
            ]


-- TODO 上記のタイプを使ったグラフのView関数を制作する。


