module StatTest exposing (..)

import Dict exposing (Dict)
import Expect exposing (equal, equalDicts, equalLists, greaterThan, lessThan, within)
import List
import Round exposing (roundNum)
import Stat exposing (average, deviation, fiducialInterval, hypothesisTesting, muFiducialInterval, shapeRetio, standardDeviation, x2Distribution, randomSampling, factorial, permutation,combination, biDistributionProbability, biDistribution)

import Test exposing (Test, describe, skip, test, todo)
import Fuzz exposing (list, float)
import Test exposing (fuzz)
import Stat exposing (coefficientOfVariation)
import Stat exposing (biDistributionProbability)
import Stat exposing (poissonDistribution)


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
                    equalLists [0.125, 0.375, 0.375, 0.125] (biDistribution 0.5 3 3)
            , test "二項分布 <= 偶数編" <|
                \_ ->
                    equalLists [0.2401, 0.4116, 0.2646, 0.0756, 0.0081] (List.map (\e -> roundNum 4 e) (biDistribution 0.7 4 4))
            , test "二項分布 <= 奇数編、その2" <|
                \_ ->
                    equalLists [ 0.064, 0.288, 0.432, 0.216 ] (List.reverse (List.map (\e -> roundNum 3 e) (biDistribution 0.6 3 3)))
            , test "ポアソン分布" <|
                \_ ->
                    equalLists [ 0.6703, 0.2681, 0.0536, 7.15e-3, 7.15e-4 ] (poissonDistribution 0.1 4)
            ]
        ]
