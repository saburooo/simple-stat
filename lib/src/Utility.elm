module Utility exposing (factorial, permutation, combination, combinationStarling, starJes, median, starling)
import Html.Attributes exposing (list)


{-| factorial
階乗計算を再帰で実装する典型的な計算式です。
例えば５人いて彼らが整列をする時、
その並び方は何通りあるか求められます。
    factorial 5

    OUT 120

-}
factorial : Int -> Int
factorial n =
    case n of
        0 ->
            1

        _ ->
            n * factorial (n - 1)


{-| permutation
順列計算、これは４人いてそのうち２人が整列するときの並び方の数

    permutation 4 2

    OUT 12

-}
permutation : Int -> Int -> Int
permutation n m =
    factorial n // factorial (n - m)


{-| combination
組み合わせ、この例だと１０人中３人が順序関係なく選ばれた場合の組み合わせの数

    combination 10 3

    OUT 120

-}
combination : Int -> Int -> Int
combination n m =
    factorial n // (factorial (n - m) * factorial m)


{-| starling
## スターリングの公式
階乗計算を指数計算で近似する公式(よく似た数値を叩き出す。)
@example starling 100
OUT 363.74
-}
starling:Int -> Float
starling n =
    let
        loge = logBase Basics.e
    in
        (toFloat n + 0.5) * ( loge (toFloat n)) - (toFloat n) + 0.5 * loge 2 * Basics.pi 


combinationStarling:Int -> Int -> Float
combinationStarling n m =
    starling n / (starling (n - m) * starling m)


{-| starJes
スタージェスの公式、これを求めることでグラフの階級数を求められます。

starJes (List.range 0 64)

OUT 7
-}
starJes : List Float -> Int
starJes argList =
    round <| 1 + logBase 2 (toFloat (List.length argList))


{-| median
中央値を求める関数、平均だと変な数字になってしまう恐れがあるため、この関数がいります。
median [1,2,3,4,5]

OUT 3
-}
median : List Float -> Float
median list =
    let
        l = List.length list
        s = List.sort list
        c = List.drop (l // 2) s |> List.head |> Maybe.withDefault 0
        cp = List.drop (l // 2 - 1) s |> List.head |> Maybe.withDefault 0 
    in
     if modBy 2 l == 0 then
        (c + cp) / 2
     else
        c
        