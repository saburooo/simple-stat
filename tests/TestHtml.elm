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
import Url

import Main


-- TODO HTMLのUIをどうする?まずはボタンを設計するか？

type Msg
    = Change String


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


mainInput : Test
mainInput =
    describe "メインファイルのインプット"
        [ describe "文字列の変換"
            [ test "文字列をリストに変更できんのか" <|
                \() ->
                    Html.input [ onInput Change ] [ ]
                        |> Query.fromHtml
                        |> Event.simulate (Event.input "1,2,3,4,5")
                        |> Event.expect (Change "1,2,3,4,5")
            ]
        ]