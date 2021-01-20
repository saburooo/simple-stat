module StatTest exposing (..)

import Dict exposing (Dict)
import Expect exposing (equal, equalDicts, equalLists, greaterThan, within)
import List
import Stat exposing (average, deviation, fiducialInterval, hypothesisTesting, muFiducialInterval, shapeRetio, standardDeviation, x2Distribution, factorial, permutation,combination, biDistributionProbability, biDistribution)

import Test exposing (Test, describe, test, todo)
import Stat exposing (coefficientOfVariation, standardNormalV)
import Stat exposing (olsRawData,poisson, sDNForDict, confidenceLimit, rawData, classifiedData, popMeanD, chiSquare, popStandardD, hypothesisForAlpha, hypothesisNotAlpha, regressionAnalysisRaw,olsClassifiedData )


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



{-
流石に長くなってきたというか分割したほうがいいので仮説検定のテストを別に建てる。
-}
hypothesisTest : Test
hypothesisTest =
    describe "仮説検定のテスト"
        [ test "母標準偏差αが既知の場合の仮説検定" <|
            \_ ->
                let
                    mu = 180
                    sigma = 20
                    n = 100
                    aveRage = 198
                    a = 0.05
                    -- TODO 有意水準の臨界値を求める関数を求める。
                    cz = 1.645
                in
                    Expect.true "従来より長くなった" (hypothesisForAlpha mu sigma n aveRage)
        , test "母標準偏差αがわからん場合の仮説検定(標本がすくない場合）" <|
            \_ ->
                Expect.true "優位に長い" (hypothesisNotAlpha 180 20 198 15 0.05)
        , test "母標準偏差αがわからん場合の仮説検定(標本が多い場合）" <|
            \_ ->
                Expect.true "優位に長い" (hypothesisNotAlpha 180 100 181 5 0.05)
        ]


{-| 
相関分析と回帰分析のテスト
-}
correlationTest:Test
correlationTest =
    describe "相関分析と回帰分析のテスト"
        [ test "単純相関のテスト（Raw dataのケース）" <|
            \_ ->
                let
                    xi = [580, 600, 470, 450, 450, 480]
                    yi = [50.1, 51.2, 46.1, 46.0, 47.0, 48.5]
                in
                    0.9373 |> within (Expect.Absolute 0.00001) (rawData xi yi)
        , test "単純相関係数の計算（Classified dataのケース）" <|
            \_ ->
                let
                    xi = [0.0, 0.5, 1.0, 1.5, 2.0, 2.5]
                    yi = [182, 168, 151, 130, 124, 120]
                    f = [2, 3, 4, 6, 5, 2]
                in
                    Dict.fromList [ ("r2", -0.967 ), ("r", 0.935)]
                        |> equalDicts (classifiedData xi yi f)
        , test "単純回帰のテスト(OLS) Raw Data" <|
            \_ ->
                let
                    xi = [5, 10, 15, 20, 25, 30]
                    yi = [13, 14, 18, 19, 22, 26]
                in
                    Dict.fromList [ ( "b", 0.5143 ), ( "a", 9.67 ), ( "r2", 0.9697 ), ( "r", 0.9847 )]
                        |> Expect.equalDicts (olsRawData xi yi)
        , test "Classified data の OLS" <|
            \_ ->
                let
                    xi = [5, 10, 15, 20, 25, 30]
                    yi = [13, 14, 18, 19, 22, 26]
                    f = [2, 3, 4, 6, 3, 2]
                in
                    Dict.fromList [("r2",0.9571),("r", 0.9783),("a",9.6363),("b",0.5050)]
                        |> equalDicts (olsClassifiedData xi yi f)
        , test "回帰係数・相関係数の計測と検定(Raw Data のケース)" <|
            \_ ->
                let
                    xi = [ 10, 20, 30, 40, 50 ]
                    yi = [ 16, 19, 28, 36, 42 ]
                in
                    Dict.fromList [ ( "a", 7.5 ), ( "b", 0.69 ), ( "ta", 4.1992 ), ( "tb", 12.813), ( "r", 0.9910 )]
                        |> equalDicts ( regressionAnalysisRaw xi yi)
        ]