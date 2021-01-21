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

import Main exposing (Route(..))
import Expect


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
