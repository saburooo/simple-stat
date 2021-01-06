module Stat exposing (..)
{-
TODO 度数と相対度数、累積度数など基本的なものを求める関数が必要だ
-}

import List exposing (sum, length, map)
import Dict exposing (Dict)
import Html.Attributes exposing (list)


-- 算術平均
average: List Float -> Float
average list
    = (sum list) / Basics.toFloat (length list)


-- 偏差(deviation)
deviation:List Float -> List Float
deviation list
    = map (\e -> e - (average list)) list


-- 標準偏差(Standard Deviation) 略してS.D.
standardDeviation:List Float -> Float
standardDeviation list
    = sqrt (sum (map (\e -> e ^ 2) (deviation list)) / Basics.toFloat (length list))


-- シャープレシオ
shapeRetio : Float -> Float -> Float -> Float
shapeRetio averageProfitability contryYield sD
    = (averageProfitability - contryYield) / sD



-- 仮説検定の英訳
hypothesisTesting : Float -> Dict String Float
hypothesisTesting n =
    let
        mu = n / 2
        sigma = (sqrt n) / 2
    in
        Dict.fromList [ ("min", (-1.96 * sigma) + mu), ("max", (1.96 * sigma) + mu)]


-- 信頼区間の英訳をググった
fiducialInterval : Float -> Float -> Dict String Float
fiducialInterval data sD =
    let
        minmu = abs ((1.96 * sD) - data)
        maxmu = abs ((-1.96 * sD) - data)
    in
        Dict.fromList [ ("min", minmu), ("max", maxmu) ]


-- 母平均はどんくらいかの信頼区間
muFiducialInterval: Float -> Float -> Float -> Dict String Float
muFiducialInterval standardAverage data sD =
    let
        minmu = abs (standardAverage + (-1.96 * ( sD / (sqrt data))))
        maxmu = abs (standardAverage + ( 1.96 * ( sD / (sqrt data))))
    in
        Dict.fromList [ ("min", minmu), ("max", maxmu) ]


-- 正直いい解決方法ではない、何故なら自由度によってこの分布は変わってしまうからだ
-- TODO 自由度によってカイ二乗分布を変えていくような関数を用意したい。
-- この数値は「完全独習　統計学入門　という書籍の自由度３のカイ二乗分布である。
pickUpDegreeOfFreedom:Int -> Float
pickUpDegreeOfFreedom x =
    let
        -- これ自由度が3の場合のカイ二乗分布だからな・・・
        xDict = Dict.fromList [
               (0, 1)
               ,(1, 0.8012)
               ,(2, 0.5724)
               ,(3, 0.3916)
               ,(4, 0.2614)
               ,(5, 0.1717)
               ,(6, 0.1116)
               ,(7, 0.0718)
               ,(8, 0.0460)
               ,(9, 0.0292)
               ,(10, 0.0185)
              ]

    in
        case x of
           1 -> Maybe.withDefault 0 (Dict.get 1 xDict)

           2 -> Maybe.withDefault 0 (Dict.get 2 xDict)

           3 -> Maybe.withDefault 0 (Dict.get 3 xDict)

           4 -> Maybe.withDefault 0 (Dict.get 4 xDict)

           5 -> Maybe.withDefault 0 (Dict.get 5 xDict)

           6 -> Maybe.withDefault 0 (Dict.get 6 xDict)

           7 -> Maybe.withDefault 0 (Dict.get 7 xDict)

           8 -> Maybe.withDefault 0 (Dict.get 8 xDict)

           9 -> Maybe.withDefault 0 (Dict.get 9 xDict)

           10 -> Maybe.withDefault 0 (Dict.get 10 xDict)

           _ -> Maybe.withDefault 0 (Dict.get 0 xDict)



-- カイ二乗分布でおそらく一番めんどくさい
x2Distribution : Int -> Int -> Float
x2Distribution x1 x2 =
  let
    z1 = pickUpDegreeOfFreedom x1
    z2 = pickUpDegreeOfFreedom x2
  in
        z1 - z2


{-
-- 標本分散 自由度を求める関数を何かしら実装する必要がある。
specimenDispersion: List Float -> Float -> Float
specimenDispersion data mu =
    let
        standard = standardDeviation data
        dev = deviation data
        v = sum (map (\e -> e ^ 2) dev)
    in
-}