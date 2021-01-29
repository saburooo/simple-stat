module Stat exposing (..)

-- TODO 実際に使う関数を厳選する。
{- モジュールを公開する気でその書き方も練習しよう

   TODO 度数と相対度数、累積度数など基本的なものを求める関数が必要だ

   # simple-stat

   @docs average

   @docs deviation
-}

import Data exposing (chiGet, standardNormalDistoributionUpper, tDistIntentionalLevelFivePer)
import Dict exposing (Dict)
import Html.Attributes exposing (list)
import List exposing (length, map, sum)
import Round exposing (roundNum)


{-| 算術平均、Float入Listを受け取ってFloatを返す
import Main exposing (Status(..))

    average [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]

    -- OUT 55

-}
average : List Float -> Float
average list =
    if length list == 0 then
        0

    else
        sum list / Basics.toFloat (length list)


{-| gigaAverage
List Floatを２つ受け取って一つのFloatを返す。

    gigaAverage [ 5, 10, 15, 20, 25, 30 ] [ 2, 3, 4, 6, 3, 2 ]

    OUT 17.75

-}
gigaAverage : List Float -> List Float -> Float
gigaAverage i f =
    (1 / List.sum f * List.sum (List.map2 (*) i f)) |> roundNum 4


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


{- シャープレシオをリストで求めるようにしたら
返されたリストで平均値を求めると０に、S.D.は１になる。
-}
shapeRetioList : List Float -> List Float
shapeRetioList dataList =
    let
        result = List.map (\data -> shapeRetio data (average dataList) (standardDeviation dataList) ) dataList
        a = average result
    in
      if isNaN a then
          []
      else
          result


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
   正確にはカイ二乗分布の表から2つ値を取り出し、引き算を行う関数

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


{-| chiSquare
カイ二乗分布を求める？関数
-}
chiSquare : List Float -> Float -> Float -> Float
chiSquare sample mu sigma =
    List.sum (List.map (\s -> (s - mu) ^ 2) sample) / (sigma ^ 2)



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


{-| popMeanD 母平均の推定
母平均を推定したいとき、標本の数によってどう計算するのかを変える。
popMeanD x s n

    OUT ( 2434.87, 2665.13 )

-}
popMeanD : Float -> Float -> Float -> ( Float, Float )
popMeanD x s n =
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


{-| popStandardD
母標準偏差の推定

    popStandardD 8 10 0.975

    OUT ( 14.605, 5.503 )

-}
popStandardD : Float -> Int -> Float -> ( Float, Float )
popStandardD s n cc =
    let
        sSquare =
            s ^ 2

        m =
            n - 1

        minMin =
            roundNum 4 (1 - cc)

        chiMax =
            chiGet m cc

        chiMin =
            chiGet m minMin

        resultOne =
            roundNum 3 (sqrt (toFloat m * sSquare / chiMax))

        resultTwo =
            roundNum 3 (sqrt (toFloat m * sSquare / chiMin))
    in
    Tuple.pair resultOne resultTwo



--------分布のための関数ここまで、--------------------------
----------色々やってくれる優しい関数


{-| factorial
階乗計算を再帰で実装する典型的な計算式です。
サンプルコードは５人いて彼らが整列をする時、
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



------色々やってくれる優しい関数ここまで


-- 仮定と分析の関数


{-| hypothesisForAlpha
仮説検定、初めてのbooleanを返す関数
例題：「新製品の寿命は１８０時間より長い（μ＞１８０）」
という仮説を立て、
標本の数１００個の平均を調べたら１９８であった。
この仮説はTrueですか？Falseですか？
hypothesisForAlpha 180 20 100 198

    OUT True

-}
hypothesisForAlpha : Float -> Float -> Int -> Float -> Bool
hypothesisForAlpha mu sigma n aveRage =
    let
        cz =
            1.645

        -- 棄却域
        criticalRegion =
            mu + cz * sigma / sqrt (toFloat n)
    in
    if aveRage > criticalRegion then
        Basics.True

    else
        Basics.False


{-| hypothesisNotAlpha
母標準偏差αがわからん場合の仮説検定
例題:標本が20で標本平均が１９８時間の電池があった
その標本標準偏差は15である。
-}
hypothesisNotAlpha : Float -> Int -> Float -> Float -> Bool
hypothesisNotAlpha mu n xi s =
    let
        ct =
            tDistIntentionalLevelFivePer (n - 1)

        xii =
            mu + ct * s / sqrt (toFloat n)
    in
    if xi > xii then
        True

    else
        False


-- TODO: この辺に分析のための補助関数を作成する。


{-| rawData
2つのリストを受け取ってr2とrに相当する値を返す。

    rawData [ 580, 600, 470, 450, 450, 480 ] [ 50.1, 51.2, 46.1, 46.0, 47.0, 48.5 ]

    OUT 0.9373

-}
rawData : List Float -> List Float -> Float
rawData xi yi =
    let
        sigmaXY =
            List.sum (List.map2 (*) xi yi)

        averageXi =
            average xi

        averageYi =
            average yi

        i =
            (sigmaXY - toFloat (length xi) * averageXi * averageYi) ^ 2

        xjj =
            List.sum (List.map (\x -> x ^ 2) xi) - toFloat (length xi) * averageXi ^ 2

        yjj =
            List.sum (List.map (\y -> y ^ 2) yi) - toFloat (length yi) * averageYi ^ 2
    in
    roundNum 4 (sqrt (i / (xjj * yjj)))


{-| classifiedData
観測したデータの数も視野に入れた統計計算

    classifiedData [ 0.0, 0.5, 1.0, 1.5, 2.0, 2.5 ] [ 182, 168, 151, 130, 124, 120 ] [ 2, 3, 4, 6, 5, 2 ]

    OUT -0.967

-}
classifiedData : List Float -> List Float -> List Float -> Dict String Float
classifiedData xi yi f =
    let
        averageXi =
            gigaAverage xi f

        averageYi =
            gigaAverage yi f

        fSum =
            List.sum f

        x2i =
            List.map (\x -> x ^ 2) xi

        y2i =
            List.map (\y -> y ^ 2) yi

        xfSum =
            List.sum (List.map2 (*) x2i f)

        yfSum =
            List.sum (List.map2 (*) y2i f)

        xyfSum =
            List.sum (List.map2 (*) xi (List.map2 (*) yi f))

        up =
            (xyfSum - fSum * averageXi * averageYi) ^ 2

        down =
            (xfSum - fSum * averageXi ^ 2) * (yfSum - fSum * averageYi ^ 2)
    in
    Dict.fromList [ ( "r2", roundNum 4 -(Basics.sqrt (up / down)) ), ( "r", roundNum 4 (up / down) ) ]


{-| olsRawData
OLS(最小2乗法)の関数

    2つのList Float を受け取って Dict String Float を返す。
    olsRawData [5, 10, 15, 20, 25, 30] [13, 14, 18, 19, 22, 26]
    -- 少数4桁ほどのほうが扱いやすそう(未検証)

    OUT [ ( "b", 0.5143 ), ( "a", 9.67 ), ( "r2", 0.9697 ), ( "r", 0.9847 )]

-}
olsRawData : List Float -> List Float -> Dict String Float
olsRawData xi yi =
    let
        xiAverage =
            average xi

        yiAverage =
            average yi

        devXi =
            deviation xi

        devYi =
            deviation yi

        momentXiYi =
            List.sum (List.map2 (*) devXi devYi)

        -- 勾配
        b =
            roundNum 4 (momentXiYi / List.sum (List.map (\x -> x ^ 2) (deviation xi)))

        -- 定数項
        a =
            roundNum 2 (yiAverage - b * xiAverage)

        rSquare =
            roundNum 4 ((momentXiYi ^ 2) / (List.sum (List.map (\x -> x ^ 2) devXi) * List.sum (List.map (\x -> x ^ 2) devYi)))
    in
    Dict.fromList [ ( "b", b ), ( "a", a ), ( "r2", rSquare ), ( "r", roundNum 4 (sqrt rSquare) ) ]


{-| olsClassifiedData

    OLS(最小2乗法)の関数のClassified data 版

    2つのList Float を受け取って Dict String Float を返す。
    olsClassifiedData [5, 10, 15, 20, 25, 30] [13, 14, 18, 19, 22, 26]
    -- 少数4桁ほどのほうが扱いやすそう(未検証)

-}
olsClassifiedData : List Float -> List Float -> List Float -> Dict String Float
olsClassifiedData xi yi f =
    let
        averageXi =
            gigaAverage xi f

        averageYi =
            gigaAverage yi f

        momentXiYi =
            List.sum (List.map2 (*) f (List.map2 (*) xi yi)) - (List.sum f * averageXi * averageYi) |> roundNum 2

        momentXi2 =
            List.sum (List.map2 (*) f (List.map (\x -> x ^ 2) xi)) - List.sum f * averageXi ^ 2 |> roundNum 2

        momentYi2 =
            List.sum (List.map2 (*) f (List.map (\y -> y ^ 2) yi)) - List.sum f * averageYi ^ 2 |> roundNum 2

        clineB =
            momentXiYi / momentXi2 |> roundNum 4

        r2 =
            (momentXiYi ^ 2) / (momentXi2 * momentYi2) |> roundNum 4

    in
        Dict.fromList [ ( "r2", r2 ), ( "r", (sqrt r2) |> roundNum 4 ), ( "b", clineB ), ( "a", averageYi - clineB * averageXi |> roundNum 4 ) ]


{-| regressionAnalysisRaw
回帰分析の関数
regressionAnalysisRaw [ 10, 20, 30, 40, 50 ] [ 16, 19, 28, 36, 42 ]

OUT Dict.fromList [ ( "a", 7.5 ), ( "b", 0.69 ), ( "ta", 4.1992 ), ( "tb", 12.813), ( "r", 0.9910 )]
-}
regressionAnalysisRaw : List Float -> List Float -> Dict String Float
regressionAnalysisRaw xi yi =
    let
        ols = olsRawData xi yi
        clineB = Maybe.withDefault 0 <| Dict.get "b" ols
        constA = Maybe.withDefault 0 <| Dict.get "a" ols
        r = Maybe.withDefault 0 <| Dict.get "r" ols
        -- 理論値 ^Y = a + bXi
        theoreticalValue = List.map (\x -> constA + clineB * x) xi
        -- 残差
        remainderSqare = List.map (\y -> y ^ 2) (List.map2 (-) yi theoreticalValue)
        sigmaTwo = (List.sum remainderSqare |> roundNum 4) / toFloat ((length yi) - 2)
        decentrationA = (1 / toFloat (length xi) + ((average xi) ^ 2) / ((List.sum (List.map (\x -> x ^ 2) xi) ) - toFloat (length xi) * (average xi) ^ 2 ) ) * sigmaTwo
        decentrationB = 1 / ((List.sum (List.map (\x -> x ^ 2) xi)) - toFloat (length xi) * (average xi) ^ 2) * sigmaTwo
        ta = constA / (sqrt decentrationA) |> roundNum 4
        tb = clineB / (sqrt decentrationB) |> roundNum 4
    in
        Dict.fromList [ ( "b", clineB ), ( "a", constA ), ("r", r ), ("ta", ta), ("tb", tb)]
