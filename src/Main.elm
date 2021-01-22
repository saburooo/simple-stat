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

import Html exposing (..)
import Html.Attributes exposing (..)

import Url
import Url.Parser exposing ((</>), s, int, top, map)
import Url.Parser exposing (Parser)
import Url.Parser exposing (oneOf)
import Url.Parser


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
    {-
    , listTwo : List Float
    , listThree : List Float
    -}
    }


-- URL
type Route
    = Top
    | Average


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ Url.Parser.map Top top
        , Url.Parser.map Average (s "average")
        ]


urlToRoute : Url -> Maybe Route
urlToRoute url =
    Url.Parser.parse routeParser url



init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg)
init flags url key =
    (Model key url listOne="", Cmd.none)


-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | OneList String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequestBrowser ->
            case urlRequestBrowser of
                -- 内部リンクならURLを更新
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url ))

                -- 外部なら普通に画面を遷移させる。
                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged urlUrl ->
            ( { model | url = urlUrl }
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
        [ text "この中から計算して欲しいものを選んでね:"
        , b [] [text (Url.toString model.url)]
        , input [ placeholder "何らかの数字を,間隔で入力してね。" value model.listOne, onInput OneList ]
        , ul []
          -- 各リンクからクリックで計算式へ
          [ viewLink "/home"
          , viewLink "/average"
          , viewLink "/population_mean"
          , viewLink "/hypothesis"
          , viewLink "/bidistribution"
          , viewLink "/mother_standard_deviation"
          , viewLink "/raw_data"
          ]
        ]
    }


viewLink: String -> Html msg
viewLink path =
  li [] [ a [ href path ] [text path ] ]
