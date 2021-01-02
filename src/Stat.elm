module Stat exposing (..)

import List exposing (sum, length, map)
import Html.Attributes exposing (list)



-- 算術平均
avarage: List Float -> Float
avarage list
    = (sum list) / Basics.toFloat (length list)


-- 偏差(deviation)
deviation:List Float -> List Float
deviation list
    = map (\e -> e - (avarage list)) list



-- 標準偏差(Standard Deviation)
standardDeviation:List Float -> Float
standardDeviation list
    = sqrt (sum (map (\e -> e * e) (deviation list)) / Basics.toFloat (length list))