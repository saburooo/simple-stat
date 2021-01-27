module Chart exposing (..)

import TypedSvg
import Svg as Svg
import TypedSvg.Attributes exposing (viewBox)
import TypedSvg exposing (polyline)
import TypedSvg.Attributes exposing (points)
import TypedSvg.Attributes exposing (fill)
import TypedSvg.Types

import Color


type Msg
    = Value1
    | Value2


-- SVG
listVisualizeArgOne: List Float -> Svg.Svg Msg
listVisualizeArgOne floatList =
    let
        floatListMap = List.map (\x -> x ^ 2 * 30) <| floatList
        tupleFloatList = List.map2 Tuple.pair floatListMap (List.sort floatList)
    in
        Svg.svg [ viewBox 0 0 800 300 ]
            [ polyline [ points tupleFloatList , fill (TypedSvg.Types.Paint Color.blue) ] [] ]
