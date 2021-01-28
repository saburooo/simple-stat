module Chart exposing (..)

import TypedSvg
import Svg as Svg
import TypedSvg.Attributes exposing (viewBox, width, height)
import TypedSvg exposing (polyline)
import TypedSvg.Attributes exposing (points)
import TypedSvg.Attributes exposing (fill)
import TypedSvg.Types as Types

import Color

import Tuple
import TypedSvg.Attributes exposing (stroke)


-- SVG
listVisualizeArgOne: List Float -> Svg.Svg msg
listVisualizeArgOne floatList =
    let
        floatListMap = List.map (\x -> x ^ 2 * 30) <| floatList
        floatRange = List.map (\x -> toFloat x * 30) (List.range 0 ( List.length floatList))
        tupleFloatList = List.map2 Tuple.pair floatRange floatListMap
    in
        Svg.svg [ viewBox 0 0 800 200 ]
            [ Svg.path [ "M 8 0 L 0 0 0 8" ] []
            , Svg.polyline [ points tupleFloatList ,fill Types.PaintNone, stroke (Types.Paint Color.blue) ] [] ]
