{-
メインモジュールのゴールはどうするか、クリックで計算の仕方を変えて、リストとか受け取って求める。
クリックするたび計算式が変わるなんてのはどうだろうか
その場合、入力した値を小数点入りの数値のリストとして受け付ける必要がある、
何を導入するか
・仮説検定
・相関分析
・回帰分析
モデルは如何定義するかはまだ検討中
-}

module Main exposing (..)


import Stat
import Browser
import Browser.Navigation as Nav

import Round exposing (roundNum)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

import Url
import Url exposing (Url)

import Url.Parser exposing ((</>), s, int, top, map)
import Url.Parser exposing (Parser)
import Url.Parser exposing (oneOf)
import Url.Parser

import Average


-- MAIN
main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL

type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , listOne : String
    , listTwo : String
    , listThree : String
    , route : Route
    }


-- URL
type Route
    = Top
    | Average


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ Url.Parser.map Top top
        , Url.Parser.map Average (Url.Parser.s "average")
        ]


urlToRoute : Url -> Maybe Route
urlToRoute url =
    Url.Parser.parse routeParser url



init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg)
init _ url key =
    (Model key url "" "" "" Top, Cmd.none)


-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | OneList String
    | TwoList String
    | ThreeList String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequestBrowser ->
            case urlRequestBrowser of
                -- 内部リンクならURLを更新
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url ))

                -- 外部なら普通に画面を遷移させる。
                _ ->
                    ( model, Cmd.none )

        UrlChanged urlUrl ->
            ( { model | url = urlUrl }
            , Cmd.none
            )
        
        OneList str ->
            ( { model | listOne = str}
            , Cmd.none
            )

        TwoList str ->
            ( { model | listTwo = str}
            , Cmd.none
            )

        ThreeList str ->
            ( { model | listThree = str}
            , Cmd.none
            )


{-|
  stringToListFloat "1,2,3,4,5"
  OUT [ 1.0, 2.0, 3.0, 4.0, 5.0 ]
-}
stringToListFloat: String -> List Float
stringToListFloat str =
    let
       strList = String.split "," str
    in
      List.map (\s -> Maybe.withDefault 0 (String.toFloat s)) strList


{-| 
受け取った List Float を文字列にしてくっつけたい
listFloatToString [1.0, 2.0, 3.0, 4.0, 5.0]
OUT "1.0, 2.0, 3.0, 4.0, 5.0"
-}
listFloatToString:List Float -> String
listFloatToString listFloat =
    let
        strChanged = List.map (\el -> String.fromFloat el) listFloat
    in
        String.join ", " strChanged

-- TODO 入力された値をリストにするのはいいとして果たしてそれがうまく行くのか


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
      Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "オレオレ統計ライブラリーデモ"
    , body =
        [ h1 [] [ text "この中から計算して欲しいものを選んでね:" ]
        , b [] [text (Url.toString model.url)]
        , br [] []
        , input [ placeholder "何らかの数字を , 間隔で入力してね。", value model.listOne, onInput OneList ] []
        , div [] [ text <| String.append "入力された値の平均値:" <| String.fromFloat <| roundNum 4 <| Stat.average <| stringToListFloat model.listOne ]
        , div [] [ text <| String.append "入力された値の偏差:" <| listFloatToString <| List.map (\x -> roundNum 4 x) <| Stat.deviation <| stringToListFloat model.listOne ]
          -- 各リンクからクリックで計算式へ
        , case model.route of
            Top ->
                div [] [ text "土器がムネムネ" ]

            Average ->
                Average.view Average.Model
        ]
    }

