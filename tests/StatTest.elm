module StatTest exposing (..)

import Dict exposing (Dict)
import Expect exposing (equal, equalDicts, equalLists, greaterThan, lessThan, within)
import List
import Round exposing (roundNum)
import Stat exposing (average, deviation, fiducialInterval, hypothesisTesting, muFiducialInterval, shapeRetio, standardDeviation, x2Distribution, randomSampling, factorial, permutation,combination, biDistributionProbability, biDistribution)

import Test exposing (Test, describe, skip, test, todo)
import Fuzz exposing (list, float)
import Test exposing (fuzz)
import Stat exposing (coefficientOfVariation, standardNormalV)
import Stat exposing (poisson, sDNForDict, confidenceLimit, popMeanD, chiSquare, popStandardD)


calcuTest : Test
calcuTest =
    describe "計算テスト"
        [ describe "sum"
            [ test "リストの合計を計算する" <|
                \_ ->
                    let
                        fifteenfive =
                            55

                        examplelist =
                            [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]
                    in
                    equal fifteenfive (List.sum examplelist)
            , test "平均計算" <|
                \_ ->
                    let
                        fifteenfive =
                            55

                        examplelist =
                            [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]

                        listavarage =
                            fifteenfive / toFloat (List.length examplelist)
                    in
                    listavarage |> within (Expect.Absolute 0.001) (average examplelist)
            , test "偏差を求める" <|
                \_ ->
                    let
                        samplelist =
                            [ 32, 27, 29, 34, 33 ]

                        resultlist =
                            [ 1, -4, -2, 3, 2 ]
                    in
                    resultlist
                        |> equalLists (deviation samplelist)
            , test "標準偏差を求める。" <|
                \_ ->
                    let
                        dataY =
                            [ 1, 2, 6, 7, 9 ]
                    in
                    greaterThan 3.03 (standardDeviation dataY)
            , test "標準偏差を求めるその2" <|
                \_ ->
                    let
                        dataX =
                            [ 4, 4, 5, 6, 6 ]
                    in
                    greaterThan 0.8 (standardDeviation dataX)
            , test "シャープレシオを求める" <|
                \_ ->
                    let
                        avarage =
                            5.0

                        sD =
                            4.5

                        kokusai =
                            3.0
                    in
                    0.44 |> within (Expect.Absolute 0.01) (shapeRetio avarage kokusai sD)
            , test "不等式で正規分布を表示" <|
                \_ ->
                    let
                        n =
                            100
                    in
                    Dict.fromList [ ( "min", 40.2 ), ( "max", 59.8 ) ]
                        |> equalDicts (hypothesisTesting n)
            , test "95%信頼区間" <|
                \_ ->
                    let
                        data =
                            20

                        sD =
                            5
                    in
                    Dict.fromList [ ( "min", 10.2 ), ( "max", 29.8 ) ]
                        |> equalDicts (fiducialInterval data sD)
            , test "母平均μの95%信頼区間" <|
                \_ ->
                    let
                        sD =
                            10

                        data =
                            25

                        standardAverage =
                            80
                    in
                    Dict.fromList [ ( "min", 76.08 ), ( "max", 83.92 ) ]
                        |> equalDicts (muFiducialInterval standardAverage data sD)
            , test "カイ二乗分布で求めるパーセンテージ" <|
                \_ ->
                    0.28 |> within (Expect.Absolute 0.01) (x2Distribution 3 6)

            , todo "標本分散を求める。"

            -- 一つの多次元リストからランダムにちゃんと値を取り出せるか
            , todo "無作為抽出"

            , test "変動係数" <|
                \_ ->
                    0.303 |> within (Expect.Absolute 0.001) (coefficientOfVariation [ 25, 18, 30, 19, 28, 40 ])
            , test "階乗" <|
                \_ ->
                    equal 120 (factorial 5)
            , test "順列" <|
                \_ ->
                    equal 720 (permutation 10 3)
            , test "10人の中で3人を選ぶ組み合わせ" <|
                \_ ->
                    equal 120 (combination 10 3)
            , test "二項分布の確率密度。" <|
                \_ ->
                    let
                        threeCointhrowZero = 0.125
                    in
                        threeCointhrowZero |> within (Expect.Absolute 0.001) (biDistributionProbability 0.5 3 0)
            , test "二項分布 <= 奇数編。" <|
                \_ ->
                    equalLists [0.125, 0.375, 0.375, 0.125] (biDistribution 0.5 3)
            , test "二項分布 <= 偶数編" <|
                \_ ->
                    equalLists [0.2401, 0.4116, 0.2646, 0.0756, 0.0081] (List.reverse (biDistribution 0.7 4))
            , test "二項分布 <= 奇数編、その2" <|
                \_ ->
                    equalLists [ 0.064, 0.288, 0.432, 0.216 ] (biDistribution 0.6 3)
            , test "ポアソン分布" <|
                \_ ->
                    equalLists [ 0.67032, 0.268128, 0.053626, 7.15e-3, 7.15e-4 ] (poisson 0.1 4)
            , test "標準化正規変数" <|
                \_ ->
                    let
                        x = 160
                        sd = 10
                    in
                        2.0 |> within (Expect.Absolute 0.1) (standardNormalV 180 x sd)
            , test "標準正規分布表をネットから取り寄せてそれをテスト" <|
                \_ ->
                    0.0062 |> within (Expect.Absolute 0.1) (sDNForDict 25 20 2)
            , test "上方信頼限界と下方信頼限界" <|
                \_ ->
                    (191.56, 148.44)
                        |> equal (confidenceLimit 170 11)
            , test "母平均 μ の推定(σが道で、標本の数が少ない場合)" <|
                \_ ->
                    let
                      n = 20
                      x = 2550
                      s = 246
                    in
                      (2434.87, 2665.13)
                          |> equal (popMeanD x s n )
            , test "母平均 μ の推定(σが道で、標本の数が多い場合)" <|
                \_ ->
                    let
                        n = 100
                        x = 2550
                        s = 246
                    in
                        (2501.78, 2598.22)
                            |> equal (popMeanD x s n)
            , test "カイ二乗分布であるx2を求める、母平均と母標準偏差" <|
                \_ ->
                    2.0918 |> within (Expect.Absolute 0.001) (chiSquare [168, 141, 157, 151, 160] 155 14 )
            , test "母標準偏差の推定" <|
                \_ ->
                    let
                        s = 8
                        n = 10
                        cc = 0.975
                    in
                        (14.605, 5.503) |> equal (popStandardD s n cc)
            ]
        ]
