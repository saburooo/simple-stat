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


-- グラフを作成するためにタイプを生成
type Graph
        = Xscale
        | Yscale


type GraphType
        = Line
        | Histogram


-- SVG


listVisualizeArgOne: List Float -> Svg.Svg msg
listVisualizeArgOne floatList =
    let
        floatListMap = List.map (\x -> x * 100) <| floatList
        floatRange = List.map (\x -> toFloat x * 100) (List.range 0 (List.length floatList))
        tupleFloatList = List.map2 Tuple.pair floatRange floatListMap
    in
        Svg.svg [ viewBox 0 0 1000 100 ]
            [ Svg.rect [ width (Types.percent 100), height (Types.percent 100), fill (Types.Paint Color.lightBlue) ] []
            , TypedSvg.g [ transform [ Types.Translate 0 100 ] ]
                [ Svg.polyline [ points tupleFloatList
                            , fill ( Types.PaintNone )
                            , stroke (Types.Paint Color.blue)
                            , TypedSvg.Attributes.style "transform: scale(1, -1)"
                            , strokeWidth (Types.px 6) ] [] 
                ]
            ]


-- TODO 上記のタイプを使ったグラフのView関数を制作する。

