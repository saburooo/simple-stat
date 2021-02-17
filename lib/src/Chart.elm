module Chart exposing (..)

import TypedSvg
import Svg as Svg
import TypedSvg.Attributes exposing (viewBox, width, height)
import TypedSvg.Attributes exposing (points)
import TypedSvg.Attributes exposing (fill, from, to)
import TypedSvg.Types as Types

import Color

import Round exposing (roundNum)

import TypedSvg.Attributes exposing (stroke)
import TypedSvg.Attributes exposing (strokeWidth)
import TypedSvg.Attributes exposing (transform)
import TypedSvg.Attributes exposing (cx, cy, r)

import TypedSvg.Attributes.InEm exposing (x1, x2, y1, y2)
import TypedSvg.Attributes.InEm as InEm
import TypedSvg.Attributes.InEm exposing (fontSize)

import Tuple
import Tuple as Tuple
import Dict exposing (Dict)

import Utility
import Svg exposing (text)
import TypedSvg.Attributes exposing (strokeDashoffset)
import TypedSvg.Attributes exposing (strokeDasharray)
import TypedSvg.Types exposing (AttributeType(..))
import TypedSvg.Types exposing (TimingValue(..))
import TypedSvg exposing (animate)
import Svg.Attributes exposing (attributeType)
import Svg.Attributes exposing (values)
import TypedSvg.Attributes exposing (dur)
import TypedSvg.Attributes exposing (attributeName)
import TypedSvg.Attributes exposing (repeatCount)
import TypedSvg.Types exposing (RepeatCount(..))
import TypedSvg.Attributes exposing (repeatDur)
import Svg.Attributes exposing (fillRule)


-- SVG


backColor: Color.Color -> Svg.Svg msg
backColor color =
    Svg.rect [ width (Types.percent 100), height (Types.percent 100), fill (Types.Paint color) ] []


lineGrid: Float -> Svg.Svg msg
lineGrid f = 
    TypedSvg.g []
    [ Svg.line [ InEm.x1 0, InEm.y1 -f, InEm.x2 100, InEm.y2 -f, strokeWidth (Types.px 1), stroke (Types.Paint Color.white) ] []
    , Svg.text_ [ InEm.x 0, InEm.y -f ] [ Svg.text <| String.fromInt <| round <| f ]
    ]


listVisualizeArgOne: List Float -> Svg.Svg msg
listVisualizeArgOne floatList =
    let
        floatListMap = ( List.map (\y -> y ^ 2 * 100) <| floatList )
        floatRange = List.map (\x -> toFloat x * 100) (List.range 1 ( (List.length floatListMap) + 1 ))
        tupleFloatList = List.map2 Tuple.pair floatRange floatListMap
        headd = Maybe.withDefault 0 (List.head floatList)
    in
        Svg.svg [ viewBox 0 0 500 250, width (Types.em 5) ]
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


{-| histgramBar is function to create a histogram
-}
histgramBar: Float -> Float -> Svg.Svg msg
histgramBar x h =
    Svg.rect [ InEm.width 1, height (Types.percent 100), InEm.x x, InEm.height h 
        , TypedSvg.Attributes.style "transform: scale(1, -1)"
        , fill (Types.Paint Color.blue)
        , strokeWidth (Types.px 2)
        , stroke ( Types.Paint Color.darkBlue )
        ] [ animate [ attributeName "height", attributeType "xml", from 0, to ( h*20 ), dur ( Types.Duration "5s" )  ] [] ]

{-| 
## appendClass is a function that determines the class and the width of the class to produce a frequency.
@example Chart.appendClass [1,2,3,4,5] == Dict.fromList [((0,1.3333333333333333),1),((1.3333333333333333,2.6666666666666665),1),((2.6666666666666665,4),1),((4,5.333333333333333),2)]
-}
appendClass: List Float -> Dict ( Float, Float ) Float 
appendClass floatList =
    let
        star = Utility.starJes floatList
        bundary = ( Maybe.withDefault 0 ( Maybe.map2 (-) ( List.maximum floatList ) ( List.minimum floatList ) ) ) / toFloat star
        classInterval = ( List.map2 (Tuple.pair) ( List.map (\s -> toFloat s * bundary |> roundNum 2) ( List.range 0 ( star + 1 ) ) ) ( List.map (\s -> toFloat s * bundary |> roundNum 2) ( List.range 1 ( star + 2 ) ) ) )
        frequencyList = ( List.map (\cls -> frequency cls floatList) classInterval )
    in
        Dict.fromList frequencyList


frequency: ( Float, Float ) -> List Float -> ( ( Float, Float ), Float )
frequency tupFloat comparisonList =
    let
        first = Tuple.first tupFloat
        second = Tuple.second tupFloat
    in
        ( tupFloat, ( List.filter (\c -> c >= first && c < second) comparisonList |> List.length |> toFloat ) )


-- リストに入る条件にあった数値を入れる。


{-| Function that takes a list and returns a histogram
listHistgram [ 1, 2, 3, 4, 5 ]
-}
listGraph: List Float -> Svg.Svg msg
listGraph floatList =
    let
        indexed = appendClass floatList
        dictRange = Dict.size indexed |> List.range 0 |> List.map (\x -> toFloat x) 
        inserted = List.map2 (Tuple.pair) dictRange (Dict.values indexed )
        graphRange = List.range 0 10 |> List.map (\x -> toFloat x)
    in
        Svg.svg [ viewBox 0 0 800 250 ]
            [ backColor Color.lightBlue
            , TypedSvg.g [ transform [ Types.Translate 0 198 ] ] ( List.map (\l -> lineGrid l) graphRange )
            , TypedSvg.g [ transform [ Types.Translate 0 198 ] ] (List.map (\h -> histgramBar ( ( Tuple.first h ) + 1 ) ( Tuple.second h )) inserted)
            , TypedSvg.text_ [ transform [ Types.Translate 0 230 ], fontSize 0.7, fill (Types.Paint Color.white) ] [ text ( listTupleStr (Dict.keys indexed) ) ]
            ]


{-| listTupleStr List (Float, Float) -> String 
listTupleStr [(0.5, 1.0)] == "0.5から1.0まで, "
-}
listTupleStr: List (Float, Float) -> String
listTupleStr listTuple =
    let
        first = List.unzip listTuple |> Tuple.first |> List.map (\f -> String.fromFloat f ++ "から")
        second = List.unzip listTuple |> Tuple.second |> List.map (\f -> String.fromFloat f ++ "まで, ")
    in
        List.map2 (++) first second |> String.concat

    
-- TODO 円グラフの作成
listCircle: List Float -> Svg.Svg msg
listCircle floatList =
    let
        indexed = appendClass floatList
        dictRange = Dict.size indexed |> List.range 0 |> List.map (\x -> toFloat x) 
        inserted = List.map2 (Tuple.pair) dictRange (Dict.values indexed )
        total = List.sum ( List.map (\c -> Tuple.second c) inserted )
    in
        Svg.svg [ viewBox 0 0 63.6619772368 63.6619772368, width (Types.em 10) ]
        [ backColor Color.lightBlue
        , TypedSvg.g [ transform [ Types.Translate -2 68 ] ] 
            ( List.map (\c -> circleMap c total) inserted )
        ]


circleMap: (Float, Float) -> Float -> Svg.Svg msg
circleMap tuple total =
    let
        first = Tuple.first tuple
        counts = Tuple.second tuple
        parcentage = 100.0 * counts / total
    in
        TypedSvg.circle
            [ cx (Types.px 31.8309886184 )
            , cy (Types.px -31.8309886184 )
            , r (Types.px 15.9154943092 )
            , fill Types.PaintNone
            , stroke ( Types.Paint ( Color.rgb ( first / 10 ) ( first / 10 ) ( first / 10 + 5 ) ) ) 
            , strokeWidth (Types.px 31.8309886184 )
            , strokeDashoffset ( 25 |> String.fromFloat )
            , strokeDasharray (( parcentage |> String.fromFloat ) ++ "," ++ ( 100.0 - parcentage |> String.fromFloat ))
            ]
            [ ]

