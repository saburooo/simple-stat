module TestHtml exposing (..)

import Test.Html.Event as Event
import Test.Html.Query as Query
import Test exposing (test)

import Html

import Stat
import Html
import Html.Events exposing (onInput)

import Test exposing (describe)
import Test exposing (Test)
import Test exposing (fuzz)

import Fuzz exposing (string)

import Url

import Main exposing (stringToListFloat)


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
        , fuzz (string) "ありとあらゆる文字列が入力されちまっても大丈夫なのか" <|
          \fuzzString ->
            fuzzString
              |> Debug.log "input"
              |> stringToListFloat
              |> Debug.log "output"
        ]
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

