module TestHtml exposing (..)

import Test.Html.Event as Event
import Test.Html.Query as Query
import Test exposing (test)

import Html

import Html
import Html.Events exposing (onInput)

import Test exposing (describe)
import Test exposing (Test)
import Test exposing (fuzz)

import Fuzz exposing (string)

import Main exposing (stringToListFloat, listFloatToString)
import Expect exposing (equal)
import Expect exposing (equalLists)


-- TODO HTMLのUIをどうする?まずはボタンを設計するか？

type Msg
    = Change String


stringTest : Test
stringTest =
    describe "文字列関係のテスト"
      [ describe "文字列を弄り倒す"
        [ test "文字列を分割してFloatへ変換" <|
          \() ->
            Html.input [ onInput Change ] [ ]
              |> Query.fromHtml
              |> Event.simulate (Event.input "1,2,3,4,5")
              |> Event.expect (Change "1,2,3,4,5")

        , fuzz string "ありとあらゆる文字列が入力されちまっても大丈夫なのか" <|
          \randomString ->
            randomString
              |> stringToListFloat
              |> Debug.log "output"
              |> Expect.atLeast []
        , test "しっかり文字列をFloat入リストに変換" <|
          \_ ->
            equalLists [1.0,2.0,3.0,4.0,5.0] (stringToListFloat "1,2,3,4,5")
        ]
        , test "今度はList Floatを文字列に変換" <|
          \_ ->
            equal "1.5,2.5,3.5,4.5,5.3" (listFloatToString [1.5,2.5,3.5,4.5,5.3])
      ]


inputTest : Test
inputTest =
    describe "Htmlのインプットをテスト"
        [ describe "ちゃんと平均って読み込んでくれんの？"
            [ test "お試しでMsgをインプット" <|
                \() ->
                    Html.input [ onInput Change ] [ ]
                        |> Query.fromHtml
                        |> Event.simulate (Event.input "平均")
                        |> Event.expect (Change "平均")
            ]
        ]

