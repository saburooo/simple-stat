module Stat exposing (..)

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
        myu = n / 2
        sigma = (sqrt n) / 2
    in
        Dict.fromList [ ("min", (-1.96 * sigma) + myu), ("max", (1.96 * sigma) + myu)]


-- 信頼区間の英訳をググった
fiducialInterval : Float -> Float -> Dict String Float
fiducialInterval data sD =
    let
        minmyu = abs ((1.96 * sD) - data)
        maxmyu = abs ((-1.96 * sD) - data)
    in
        Dict.fromList [ ("min", minmyu), ("max", maxmyu) ]