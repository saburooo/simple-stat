module StatTest exposing (..)

import Expect exposing (equal, within, equalLists)
import Test exposing (describe, test, Test, todo)
import List
import Stat exposing (average)
import Stat exposing (deviation)
import Stat exposing (standardDeviation, fiducialInterval)
import Stat exposing (shapeRetio, hypothesisTesting)
import Expect exposing (lessThan)
import Expect exposing (greaterThan)
import Dict exposing (Dict)
import Expect exposing (equalDicts)
import Stat exposing (hypothesisTesting)


calcuTest : Test
calcuTest =
    describe "計算テスト"
        [ describe "sum"
            [ test "リストの合計を計算する" <|
                \_ ->
                    let
                        fifteenfive = 55
                        examplelist = [1,2,3,4,5,6,7,8,9,10]
                    in
                        equal fifteenfive (List.sum examplelist)
            , test "平均計算" <|
                \_ ->
                    let
                        fifteenfive = 55
                        examplelist = [1,2,3,4,5,6,7,8,9,10]
                        listavarage = fifteenfive / toFloat (List.length examplelist)
                    in
                        listavarage |> within (Expect.Absolute 0.001) (average examplelist)
            , test "偏差を求める" <|
                \_ ->
                    let
                        samplelist = [32, 27, 29, 34, 33]
                        resultlist = [1, -4, -2, 3, 2]
                    in
                        resultlist
                            |> equalLists (deviation samplelist) 
            , test "標準偏差を求める。" <|
                \_ ->
                    let
                        dataY = [1, 2, 6, 7, 9]
                    in
                        greaterThan 3.03 (standardDeviation dataY)
            , test "標準偏差を求めるその2" <|
                \_ ->
                    let
                        dataX = [4, 4, 5, 6, 6]
                    in
                        greaterThan 0.8 (standardDeviation dataX)
            , test "シャープレシオを求める" <|
                \_ ->
                    let
                        avarage = 5.0
                        sD = 4.5
                        kokusai = 3.0
                    in
                        0.44 |> within (Expect.Absolute 0.01) (shapeRetio avarage kokusai sD)
            , test "不等式で正規分布を表示" <|
                \_ ->
                    let
                        n = 100
                    in
                        Dict.fromList [ ("min" ,40.2), ( "max", 59.8)]
                            |> equalDicts (hypothesisTesting n)
            , test "95%信頼区間" <|
                \_ ->
                    let
                        data = 20
                        sD = 5
                    in
                        Dict.fromList [ ("min", 10.2), ("max", 29.8) ]
                            |> equalDicts (fiducialInterval data sD)
            ]
        ]