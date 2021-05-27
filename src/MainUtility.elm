module MainUtility exposing (..)

import Dict exposing (Dict)

-- Utility


{-|
  stringToListFloat "1,2,3,4,5"
  OUT [ 1.0, 2.0, 3.0, 4.0, 5.0 ]
-}
stringToListFloat: String -> List Float
stringToListFloat str =
    let
       strList = String.split "," str
       digitList = List.map (\s -> String.filter Char.isDigit s) strList
    in
      List.map (\s -> Maybe.withDefault 0 (String.toFloat s)) digitList
        -- これだと０の値が本当に入っている場合に計算できない。


{-|
受け取った List Float を文字列にしてくっつけたい
listFloatToString [1.0, 2.0, 3.0, 4.0, 5.0]
OUT "1.0, 2.0, 3.0, 4.0, 5.0"
-}
listFloatToString:List Float -> String
listFloatToString listFloat =
    let
        strChanged = List.map (\el -> String.fromFloat el) listFloat
    in
        String.join ", " strChanged

-- TODO 入力された値をリストにするのはいいとして果たしてそれがうまく行くのか

dictInFloatToString: Dict String Float -> String -> Float
dictInFloatToString dictFS str =
  Maybe.withDefault 0 (Dict.get str dictFS)


strToIntList: String -> List Int
strToIntList str =
    let
        splited = String.split "," str
    in
        List.map (\s -> Maybe.withDefault 0 (String.toInt s)) splited
    


