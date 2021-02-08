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
import TypedSvg.Attributes.InEm as InEm
import TypedSvg.Attributes.InEm exposing (fontSize)


-- グラフを作成するためにタイプを生成
type Graph
        = Xscale
        | Yscale


-- SVG


backColor: Color.Color -> Svg.Svg msg
backColor color =
    Svg.rect [ width (Types.percent 100), height (Types.percent 100), fill (Types.Paint color) ] []


listVisualizeArgOne: List Float -> Svg.Svg msg
listVisualizeArgOne floatList =
    let
        floatListMap = ( List.map (\y -> y ^ 2 * 100) <| floatList )
        floatRange = List.map (\x -> toFloat x * 100) (List.range 1 ( (List.length floatListMap) + 1 ))
        tupleFloatList = List.map2 Tuple.pair floatRange floatListMap
        headd = Maybe.withDefault 1 ( List.head floatList )
    in
        Svg.svg [ viewBox 0 0 500 250 ]
            [ backColor Color.lightBlue
            , TypedSvg.g [ transform [ Types.Translate 0 200 ] ]
                [ Svg.line [ x1 0, y1 0, x2 500, y2 0, strokeWidth (Types.px 3), stroke (Types.Paint Color.white) ] []
                , Svg.line [ x1 5.1, y1 200, x2 5.1, y2 -120, strokeWidth (Types.px 3), stroke (Types.Paint Color.white) ] []
                , Svg.line [ InEm.x1 4.8, InEm.y1 -5, InEm.x2 5.4, InEm.y2 -5, strokeWidth (Types.px 3), stroke (Types.Paint Color.white) ] []
                , Svg.text_ [ InEm.x 5.5, InEm.y 1.1, fontSize 0.8, fill (Types.Paint Color.darkBlue) ] [ Svg.text "0" ]
                , Svg.text_ [ InEm.x 5.5, InEm.y -6, fontSize 0.8, fill (Types.Paint Color.darkBlue) ] [ Svg.text "1" ]
                , Svg.text_ [ InEm.x 0, InEm.y -11, fontSize 0.8, fill (Types.Paint Color.darkBlue) ] [ Svg.text ("最初の値：" ++ String.fromFloat headd ) ]
                , Svg.polyline [ points tupleFloatList
                            , fill ( Types.PaintNone )
                            , stroke (Types.Paint Color.blue)
                            , TypedSvg.Attributes.style "transform: scale(1, -1)"
                            , strokeWidth (Types.px 6) ] [] 
                ]
            ]


-- TODO 上記のタイプを使ったグラフのView関数を制作する。


listHistgram:List Float -> Svg.Svg msg
listHistgram floatList =
    let
        bundary = ( Maybe.withDefault 0 ( List.maximum floatList ) - Maybe.withDefault 0 ( List.minimum floatList ) ) / toFloat (List.length floatList)
        starJess = Utility.starJes floatList
    in
        Svg.svg [ viewBox 0 0 500 500 ] [
            Svg.text_ [InEm.x 5.5, InEm.y 1.1, fontSize 0.8, fill (Types.Paint Color.darkBlue)] [
                Svg.text ( (String.fromFloat bundary) ++ "と" ++ (String.fromInt starJess) )
            ]
        ]
