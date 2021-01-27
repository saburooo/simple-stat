module Chart exposing (..)

import TypedSvg
import Svg as Svg
import TypedSvg.Attributes exposing (viewBox)
import TypedSvg exposing (polyline)
import TypedSvg.Attributes exposing (points)
import TypedSvg.Attributes exposing (fill)
import TypedSvg.Types

import Color

import Tuple
import TypedSvg.Attributes exposing (stroke)


-- SVG
listVisualizeArgOne: List Float -> Svg.Svg msg
listVisualizeArgOne floatList =
    let
        floatListMap = List.map (\x -> x ^ 2 * 100) <| floatList
        floatRange = List.map (\x -> toFloat x) (List.range 0 ( List.length floatList))
        tupleFloatList = List.map2 Tuple.pair floatRange floatListMap
    in
        Svg.svg [ viewBox 0 0 800 500 ]
            [ Svg.polyline [ points tupleFloatList ,fill TypedSvg.Types.PaintNone, stroke (TypedSvg.Types.Paint Color.blue) ] [] ]
