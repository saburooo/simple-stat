module Stat exposing (..)

import List exposing (sum, length, map)
import Dict exposing (Dict)
import Html.Attributes exposing (list)



-- 算術平均
average: List Float -> Maybe Float
average list
    = (sum list) / Basics.toFloat (length list)



-- 偏差(deviation)
deviation:List Float -> List Float
deviation list
    = map (\e -> e - (average list)) list



-- 標準偏差(Standard Deviation) 略してS.D.
standardDeviation:List Float -> Float
standardDeviation list
    = sqrt (sum (map (\e -> e * e) (deviation list)) / Basics.toFloat (length list))



{-
（データ　−　平均値）÷　S.D.　＝　一個分以上離れていたら特殊なデータ
それを応用したシャープレシオというものあり
-}

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



-- カイ二乗分布でおそらく一番めんどくさい
{-
x2Distribution : Int -> Int -> Maybe Float
x2Distribution x1 x2 =
    let
        z1 = case x1 of
            3 -> 0.3916 

            _ -> 1
        
        z2 = case x2 of
            6 = 
    in
-}