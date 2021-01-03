module Stat exposing (..)

import List exposing (sum, length, map)
import Html.Attributes exposing (list)



-- 算術平均
average: List Float -> Float
average list
    = (sum list) / Basics.toFloat (length list)



-- 偏差(deviation)
deviation:List Float -> List Float
deviation list
    = map (\e -> e - (average list)) list



-- 標準偏差(Standard Deviation)
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
