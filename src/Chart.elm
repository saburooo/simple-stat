module Chart exposing (..)

import TypedSvg
import Svg as Svg
import TypedSvg.Attributes exposing (viewBox, width, height)
import TypedSvg.Attributes exposing (points)
import TypedSvg.Attributes exposing (fill)
import TypedSvg.Types as Types

import Color

import Round exposing (roundNum)

import TypedSvg.Attributes exposing (stroke)
import TypedSvg.Attributes exposing (strokeWidth)
import TypedSvg.Attributes exposing (transform)

import TypedSvg.Attributes.InEm exposing (x1, x2, y1, y2)
import TypedSvg.Attributes.InEm as InEm
import TypedSvg.Attributes.InEm exposing (fontSize)

import Tuple
import Tuple as Tuple
import Dict exposing (Dict)

import Utility

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
        headd = Maybe.withDefault 0 (List.head floatList)
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


histgramBar: Float -> Float -> Svg.Svg msg
histgramBar x h =
    Svg.rect [ InEm.width 1, height (Types.percent 100), InEm.x x, InEm.height h 
        , TypedSvg.Attributes.style "transform: scale(1, -1)"
        , fill (Types.Paint Color.blue)
        , strokeWidth (Types.px 2)
        , stroke ( Types.Paint Color.darkBlue )
        ] []


appendClass: List Float -> Dict ( Float, Float ) Int
appendClass floatList =
    let
        star = Utility.starJes floatList
        bundary = ( Maybe.withDefault 0 ( List.maximum floatList ) - ( Maybe.withDefault 0 ( List.minimum floatList ) ) ) / toFloat star
        classInterval = ( List.map2 (Tuple.pair) ( List.map (\s -> toFloat s * bundary) ( List.range 0 star ) ) ( List.map (\s -> toFloat s * bundary) ( List.range 1 ( star + 1 ) ) ) )
    in
        Dict.fromList ( List.map (\cls -> frequency cls floatList) classInterval )


frequency: ( Float, Float ) -> List Float -> List ( ( Float, Float ), Int )
frequency tupFloat comparisonList =
    let
        first = Tuple.first tupFloat
        second = Tuple.second tupFloat
    in
        List.singleton ( tupFloat, ( List.filter (\c -> c >= first && c < second) comparisonList |> List.length ) )


-- リストに入る条件にあった数値を入れる。


listHistgram:List Float -> Svg.Svg msg
listHistgram floatList =
    let
        indexed = List.indexedMap Tuple.pair floatList 
    in
        Svg.svg [ viewBox 0 0 500 200 ]
          [ backColor Color.lightBlue
          , TypedSvg.g [ transform [ Types.Translate 0 199 ] ] (List.map (\a -> histgramBar ( toFloat ( Tuple.first a ) ) ( sqrt ( Tuple.second a ^ 2 ) )) indexed)
          ]
