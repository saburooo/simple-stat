module Stat exposing (..)

{- モジュールを公開する気でその書き方も練習しよう

   TODO 度数と相対度数、累積度数など基本的なものを求める関数が必要だ

   # simple-stat

   @docs average

   @docs deviation
-}

import Data exposing (standardNormalDistoributionUpper, tDistIntentionalLevelFivePer)
import Dict exposing (Dict, foldl, foldr)
import Html exposing (i)
import Html.Attributes exposing (list)
import List exposing (length, map, sum)
import Round exposing (roundNum)
import String exposing (toInt)


{-| 算術平均、Float入Listを受け取ってFloatを返す

    average [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]

    -- OUT 55

-}
average : List Float -> Float
average list =
    if length list == 0 then
        0

    else
        sum list / Basics.toFloat (length list)


{-| 偏差(deviation)
サンプリングしたデータをその平均で引く

    deviation [ 32, 27, 29, 34, 33 ]

    -- OUT [ 1, -4, -2, 3, 2 ]

-}
deviation : List Float -> List Float
deviation list =
    map (\e -> e - average list) list



{- 標準偏差(Standard Deviation) 略してS.D.

   standardDeviation [ 1, 2, 6, 7, 9 ]

   greaterThan 3.03
-}


standardDeviation : List Float -> Float
standardDeviation list =
    sqrt (sum (map (\e -> e ^ 2) (deviation list)) / Basics.toFloat (length list - 1))


{-| 変動係数
標準偏差を平均で割ると算出できる。
一つのリストで求められる

    coefficientOfVariation [ 25, 18, 30, 19, 28, 40 ]

    -- OUT 0.303

-}
coefficientOfVariation : List Float -> Float
coefficientOfVariation sampleData =
    standardDeviation sampleData / average sampleData



{- シャープレシオ

   shapeRetio 5.0 3.0 4.5

   -- OUT 0.44
-}


shapeRetio : Float -> Float -> Float -> Float
shapeRetio averageProfitability contryYield sD =
    (averageProfitability - contryYield) / sD



{- 仮説検定の英訳、もっといい英訳があるかもしれない

   hypothesisTesting 100

   -- OUT Dict.fromList [ ( "min", 40.2 ), ( "max", 59.8 ) ]
-}


hypothesisTesting : Float -> Dict String Float
hypothesisTesting n =
    let
        mu =
            n / 2

        sigma =
            sqrt n / 2
    in
    Dict.fromList [ ( "min", (-1.96 * sigma) + mu ), ( "max", (1.96 * sigma) + mu ) ]



{- 信頼区間の英訳をググった

   fiducialInterval 20 5

   -- OUT Dict.fromList [ ( "min", 10.2 ), ( "max", 29.8 ) ]
-}


fiducialInterval : Float -> Float -> Dict String Float
fiducialInterval data sD =
    let
        minmu =
            abs ((1.96 * sD) - data)

        maxmu =
            abs ((-1.96 * sD) - data)
    in
    Dict.fromList [ ( "min", minmu ), ( "max", maxmu ) ]



{- 母平均はどんくらいかの信頼区間

   muFiducialInterval 80 10 25

   -- OUT Dict.fromList [ ( "min", 76.08 ), ( "max", 83.92 ) ]
-}


muFiducialInterval : Float -> Float -> Float -> Dict String Float
muFiducialInterval standardAverage data sD =
    let
        minmu =
            abs (standardAverage + (-1.96 * (sD / sqrt data)))

        maxmu =
            abs (standardAverage + (1.96 * (sD / sqrt data)))
    in
    Dict.fromList [ ( "min", minmu ), ( "max", maxmu ) ]



{- 自由度からカイ二乗分布を引っ張り出す関数
   -- 正直いい解決方法ではない、何故なら自由度によってこの分布は変わってしまうからだ
   -- TODO 自由度によってカイ二乗分布を変えていくような関数を用意したい。
   -- この数値は「完全独習　統計学入門」という書籍の自由度３のカイ二乗分布である。

       pickUpDegreeOfFreedom 1

       -- OUT 0.8012

-}


pickUpDegreeOfFreedom : Int -> Float
pickUpDegreeOfFreedom x =
    let
        -- これ自由度が3の場合のカイ二乗分布だからな・・・
        xDict =
            Dict.fromList
                [ ( 0, 1 )
                , ( 1, 0.8012 )
                , ( 2, 0.5724 )
                , ( 3, 0.3916 )
                , ( 4, 0.2614 )
                , ( 5, 0.1717 )
                , ( 6, 0.1116 )
                , ( 7, 0.0718 )
                , ( 8, 0.046 )
                , ( 9, 0.0292 )
                , ( 10, 0.0185 )
                ]
    in
    case x of
        1 ->
            Maybe.withDefault 0 (Dict.get 1 xDict)

        2 ->
            Maybe.withDefault 0 (Dict.get 2 xDict)

        3 ->
            Maybe.withDefault 0 (Dict.get 3 xDict)

        4 ->
            Maybe.withDefault 0 (Dict.get 4 xDict)

        5 ->
            Maybe.withDefault 0 (Dict.get 5 xDict)

        6 ->
            Maybe.withDefault 0 (Dict.get 6 xDict)

        7 ->
            Maybe.withDefault 0 (Dict.get 7 xDict)

        8 ->
            Maybe.withDefault 0 (Dict.get 8 xDict)

        9 ->
            Maybe.withDefault 0 (Dict.get 9 xDict)

        10 ->
            Maybe.withDefault 0 (Dict.get 10 xDict)

        _ ->
            Maybe.withDefault 0 (Dict.get 0 xDict)



{- カイ二乗分布でおそらく一番実装が大変なことになる。
   正確にはカイ二乗分布から2つ値を取り出し、引き算を行う関数

       x2Distribution 3 6

       -- OUT 0.28
-}


x2Distribution : Int -> Int -> Float
x2Distribution x1 x2 =
    let
        z1 =
            pickUpDegreeOfFreedom x1

        z2 =
            pickUpDegreeOfFreedom x2
    in
    z1 - z2



-- 標本分散 自由度を求める関数とそこからカイ二乗分布を割り出す関数を何かしら実装する必要がある。


specimenDispersion : List Float -> Float -> Float
specimenDispersion data mu =
    Debug.todo "カイ二乗分布をうまく求める方法を理解する。"



-- RANDOM


{-| 無作為抽出
-}
randomSampling : List (List Float) -> Int -> List Float
randomSampling multidimensionArray sampleNumber =
    Debug.todo "どうやって多次元配列から抜き出せばよいだろうか"



---------------------------------------------- 後で分割するかもしれないが、まだまだファイルは小さいのでまとめて書く ----------------------------------------------
-- 確率


{-| biDistributionProbability
二項分布の確率関数

    biDistributionProbability 0.5 3 0

    OUT 0.125

-}
biDistributionProbability : Float -> Int -> Int -> Float
biDistributionProbability p n c =
    (toFloat (combination n c) / 1) * (p ^ toFloat c) * ((1 - p) ^ toFloat (n - c))


biDistributionRecursion : Float -> Int -> Int -> List Float
biDistributionRecursion p n c =
    case c of
        0 ->
            biDistributionProbability p n c :: []

        _ ->
            biDistributionProbability p n c :: biDistributionRecursion p n (c - 1)


{-| biDistribution
二項分布、再帰で求めるその数をなんとかそれっぽくするため
ひっくり返します。
しかし奇数のときはひっくり返さないほうが良さそう？(要検証)

    biDistribution 0.7 4

    OUT [ 0.2401, 0.4116, 0.2646, 0.0756, 0.0081 ]

    biDistribution 0.5 3 3

    OUT [ 0.125, 0.375, 0.375, 0.125 ]

-}
biDistribution : Float -> Int -> List Float
biDistribution p n =
    List.reverse (List.map (\x -> roundNum n x) (biDistributionRecursion p n n))


{-| poissonDistributionProbability
ポアソン分布の確率密度関数

    poissonDistributionProbability 0.1, 4

    OUT 7.15e-4

-}
poissonDistributionProbability : Float -> Int -> Int -> Float
poissonDistributionProbability p n c =
    let
        nn =
            toFloat n

        mu =
            p * nn

        el =
            Basics.e ^ -mu
    in
    el * (mu ^ toFloat c) / toFloat (factorial c)


poissonDistributionRecursion : Float -> Int -> Int -> List Float
poissonDistributionRecursion p n c =
    case c of
        0 ->
            poissonDistributionProbability p n c :: []

        _ ->
            poissonDistributionProbability p n c :: poissonDistributionRecursion p n (c - 1)


{-| poisson
再帰の都合上逆向きになるので逆さまにひっくり返す。

    poisson 0.1 4

    OUT [ 0.6703, 0.2681, 0.0536, 7.15e-3, 7.15e-4 ]

-}
poisson : Float -> Int -> List Float
poisson p n =
    List.reverse (List.map (\x -> roundNum 6 x) (poissonDistributionRecursion p n n))


{-| standardNormalV
正規分布の標準化を求める
どんな正規分布もN（0， 1 ＾　2）の正規分布になれる。

    standardNormalV 180 160 10

    OUT 2.0

-}
standardNormalV : Float -> Float -> Float -> Float
standardNormalV data x sd =
    (data - x) / sd


{-| sDNForDict
Dictな標準正規分布表から数値を取り出す。
sDNForDict 25 20 2

    OUT 0.0062
    TODO:リストから該当する数値を取り出したいのだがうまいやり方が思いつかないのでしばらくこのままかな？

-}
sDNForDict : Float -> Float -> Float -> Float
sDNForDict data x sd =
    Maybe.withDefault 0 (List.head (Maybe.withDefault [] (Dict.get (standardNormalV data x sd) standardNormalDistoributionUpper)))


{-| confidenceLimit
信頼限界
２つのFloatを受け取ってFloat２つ入りのTupleを返す。
信頼係数を95%とするけど３段階に分けるようにしても面白いかもしれない。

    confidenceLimit 170 11

    OUT ( 191.56, 148.44 )

-}
confidenceLimit : Float -> Float -> ( Float, Float )
confidenceLimit x sigma =
    let
        upper =
            1.96 * sigma + x

        lower =
            -1.96 * sigma + x
    in
    Tuple.pair upper lower


{-| popMearnD 母平均の推定
母平均を推定したいとき、標本の数によってどう計算するのかを変える。
popMearnD x s n

    OUT ( 2434.87, 2665.13 )

-}
popMearnD : Float -> Float -> Float -> ( Float, Float )
popMearnD x s n =
    let
        degreeOfFreedom =
            n - 1

        t =
            tDistIntentionalLevelFivePer (floor degreeOfFreedom)

        upper =
            roundNum 2 (x + t * s / sqrt n)

        lower =
            roundNum 2 (x - t * s / sqrt n)
    in
    Tuple.pair lower upper



--------分布のための関数ここまで、--------------------------
----------色々やってくれる優しい関数


{-| factorial
階乗計算で再帰で実装する典型的な計算式です。

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
順列

    permutation 4 2

    OUT 12

-}
permutation : Int -> Int -> Int
permutation n m =
    factorial n // factorial (n - m)


{-| combination
組み合わせ

    combination 10 3

    OUT 120

-}
combination : Int -> Int -> Int
combination n m =
    factorial n // (factorial (n - m) * factorial m)
