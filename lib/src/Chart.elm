module Chart exposing (..)

import TypedSvg
import Svg as Svg

import TypedSvg.Types as Types

import Color

import Round exposing (roundNum)
import TypedSvg.Attributes.InEm as InEm
import TypedSvg.Attributes.InEm exposing (fontSize)

import Tuple
import Dict exposing (Dict)

import Utility

import TypedSvg.Attributes as TypedSvgAttri

import TypedSvg.Types exposing (AttributeType(..))
import TypedSvg.Types exposing (TimingValue(..))
import TypedSvg.Types exposing (RepeatCount(..))

import Svg.Attributes exposing (attributeType)


-- SVG


backColor: Color.Color -> Svg.Svg msg
backColor color =
    Svg.rect [ TypedSvgAttri.width (Types.percent 100), TypedSvgAttri.height (Types.percent 100), TypedSvgAttri.fill (Types.Paint color) ] []


lineGrid: Float -> Svg.Svg msg
lineGrid f = 
    TypedSvg.g []
    [ Svg.line [ InEm.x1 0, InEm.y1 -f, InEm.x2 100, InEm.y2 -f, TypedSvgAttri.strokeWidth (Types.px 1), TypedSvgAttri.stroke (Types.Paint Color.white) ] []
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
        Svg.svg [ TypedSvgAttri.viewBox 0 0 500 250, TypedSvgAttri.width (Types.em 5) ]
            [ backColor Color.lightBlue
            , TypedSvg.g [ TypedSvgAttri.transform [ Types.Translate 0 200 ] ]
                [ Svg.line [ InEm.x1 0, InEm.y1 0, InEm.x2 500, InEm.y2 0, TypedSvgAttri.strokeWidth (Types.px 3), TypedSvgAttri.stroke (Types.Paint Color.white) ] []
                , Svg.line [ InEm.x1 5.1, InEm.y1 200, InEm.x2 5.1, InEm.y2 -120, TypedSvgAttri.strokeWidth (Types.px 3), TypedSvgAttri.stroke (Types.Paint Color.white) ] []
                , Svg.line [ InEm.x1 4.8, InEm.y1 -5, InEm.x2 5.4, InEm.y2 -5, TypedSvgAttri.strokeWidth (Types.px 3), TypedSvgAttri.stroke (Types.Paint Color.white) ] []
                , Svg.text_ [ InEm.x 5.5, InEm.y 1.1, fontSize 0.8, TypedSvgAttri.fill (Types.Paint Color.darkBlue) ] [ Svg.text "0" ]
                , Svg.text_ [ InEm.x 5.5, InEm.y -6, fontSize 0.8, TypedSvgAttri.fill (Types.Paint Color.darkBlue) ] [ Svg.text "1" ]
                , Svg.text_ [ InEm.x 0, InEm.y -11, fontSize 0.8, TypedSvgAttri.fill (Types.Paint Color.darkBlue) ] [ Svg.text ("最初の値：" ++ String.fromFloat headd ) ]
                , Svg.polyline [ TypedSvgAttri.points tupleFloatList
                            , TypedSvgAttri.fill ( Types.PaintNone )
                            , TypedSvgAttri.stroke (Types.Paint Color.blue)
                            , TypedSvgAttri.style "transform: scale(1, -1)"
                            , TypedSvgAttri.strokeWidth (Types.px 6) ] []
                ]
            ]


-- TODO 上記のタイプを使ったグラフのView関数を制作する。


{-| histgramBar is function to create a histogram
-}
histgramBar: Float -> Float -> Svg.Svg msg
histgramBar x h =
    Svg.rect [ InEm.width 1, TypedSvgAttri.height (Types.percent 100), InEm.x x, InEm.height h 
        , TypedSvgAttri.style "transform: scale(1, -1)"
        , TypedSvgAttri.fill (Types.Paint Color.blue)
        , TypedSvgAttri.strokeWidth (Types.px 2)
        , TypedSvgAttri.stroke ( Types.Paint Color.darkBlue )
        ] [ TypedSvg.animate [ TypedSvgAttri.attributeName "height", attributeType "xml", TypedSvgAttri.from 0, TypedSvgAttri.to ( h*20 ), TypedSvgAttri.dur ( Types.Duration "5s" )  ] [] ]

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
        Svg.svg [ TypedSvgAttri.viewBox 0 0 800 250 ]
            [ backColor Color.lightBlue
            , TypedSvg.g [ TypedSvgAttri.transform [ Types.Translate 0 198 ] ] ( List.map (\l -> lineGrid l) graphRange )
            , TypedSvg.g [ TypedSvgAttri.transform [ Types.Translate 0 198 ] ] (List.map (\h -> histgramBar ( ( Tuple.first h ) + 1 ) ( Tuple.second h )) inserted)
            , TypedSvg.text_ [ TypedSvgAttri.transform [ Types.Translate 0 230 ], fontSize 0.7, TypedSvgAttri.fill (Types.Paint Color.white) ] [ Svg.text ( listTupleStr (Dict.keys indexed) ) ]
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
        Svg.svg [ TypedSvgAttri.viewBox 0 0 63.6619772368 63.6619772368, TypedSvgAttri.width (Types.px 300) ]
        [ backColor Color.lightBlue
        , TypedSvg.g [ TypedSvgAttri.transform [ Types.Translate -0.1 63.5 ] ]
            ( List.map (\c -> circleMap c total (offsetPickUp c inserted)) inserted )
        ]


offsetPickUp: (Float, Float) -> List (Float, Float) -> Float
offsetPickUp tuple tupleList =
    let
        total = List.sum (List.map (\c -> Tuple.second c) tupleList)
        pickUp = List.filter (\c -> Tuple.first c <= Tuple.first tuple) tupleList
    in
        List.foldl (+) 0 (List.map (\c -> Tuple.second c) pickUp) * 100 / total |> roundNum 2


circleMap: (Float, Float) -> Float -> Float -> Svg.Svg msg
circleMap tuple total offset =
    let
        first = Tuple.first tuple
        counts = Tuple.second tuple
        parcentage = 100.0 * counts / total
    in
        TypedSvg.circle
            [ TypedSvgAttri.cx (Types.px 31.8309886184 )
            , TypedSvgAttri.cy (Types.px -31.8309886184 )
            , TypedSvgAttri.r (Types.px 15.9154943092 )
            , TypedSvgAttri.fill Types.PaintNone
            , TypedSvgAttri.stroke ( Types.Paint ( Color.rgb ( first / 10 ) ( first / 10 + 0.1 ) ( first / 10 + 0.25 ) ) ) 
            , TypedSvgAttri.strokeWidth (Types.px 31.8309886184 )
            , TypedSvgAttri.strokeDashoffset ( 25 - offset + parcentage |> String.fromFloat )
            , TypedSvgAttri.strokeDasharray (( parcentage |> String.fromFloat ) ++ "," ++ ( 100.0 - parcentage |> String.fromFloat ))
            ]
            [ ]

