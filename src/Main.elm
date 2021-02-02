{-
メインモジュールのゴールはどうするか、クリックで計算の仕方を変えて、リストとか受け取って求める。
その場合、入力した値を小数点入りの数値のリストとして受け付ける必要がある、
・仮説検定
・相関分析
・回帰分析
あたり？
-}
-- TODO: どうやって某アナリティクス風のUIにしようか

module Main exposing (..)


import Stat
import Utility
import Browser

import Round exposing (roundNum)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

import Dict exposing (Dict)
import List
import Html.Events exposing (onClick)
import Chart
import Html
import Html
import Html
import Html
import Html


-- MAIN
main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL

type alias Model =
    { listOne : String
    , listTwo : String
    , listThree : String
    , route : Route
    }


-- URL
type Route
    = Top
    | Ols
    | Regres
    | Dist
    | Parcen
    | Hypo


init : () -> ( Model, Cmd Msg)
init _  =
    (Model "" "" "" Top, Cmd.none)


-- UPDATE


type Msg
    = OneList String
    | TwoList String
    | ThreeList String
    | TopPage
    | OLS
    | Regression
    | Distribution
    | Parcentage
    | Hypothesis


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

        TopPage ->
            ( { model | route = Top }
            , Cmd.none
            )

        OLS ->
            ( { model | route = Ols }
            , Cmd.none
            )

        Regression ->
            ( { model | route = Regres }
            , Cmd.none
            )

        Distribution ->
            ( { model | route = Dist }
            , Cmd.none
            )

        Parcentage ->
            ( { model | route = Parcen }
            , Cmd.none
            )

        Hypothesis ->
            ( { model | route = Hypo }
            , Cmd.none
            )


-- Utility


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
        -- これだと０の値が本当に入っている場合に計算できない。
        |> List.filter (\x -> x /= 0)


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

dictInFloatToString: Dict String Float -> String -> Float
dictInFloatToString dictFS str =
  Maybe.withDefault 0 (Dict.get str dictFS)


strToInt: String -> Int
strToInt str =
    let
        splited = String.split "," str
        castedInt = List.map (\s -> Maybe.withDefault 0 (String.toInt s)) splited
    in
        List.sum castedInt
    


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
      Sub.none


-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "オレの統計ライブラリーデモ"
    , body =
        -- 一番上にリンクを貼ってなんとかする。
        [
            div [ class "columns" ]
            [
                div [ class "column is-one-thirds panel is-info" ]
                [ h1 [ class "title panel-heading"] [ text "メニューです" ]
                    , nav [ class "nav" ] 
                        [ ul [ class "panel-headering" ]
                            [ buttonLink TopPage "平均値"
                            , buttonLink Parcentage "確率"
                            , buttonLink OLS "OLS最小二乗法"
                            , buttonLink Regression "回帰分析"
                            , buttonLink Distribution "二項分布・ポアソン分布"
                            , buttonLink Hypothesis "仮説検定"
                            ]
                        ]
                ]
                , div [ class "column is-two-thirds" ]
                    [ div [ class "message is-medium is-info" ] [ topView model ]
                    , br [] []
                    ]
            ]
        ]
    }


topView:Model -> Html Msg
topView model =
    let
        one = stringToListFloat model.listOne
        two = stringToListFloat model.listTwo
        three = stringToListFloat model.listThree
        oneInt = strToInt model.listOne
        twoInt = strToInt model.listTwo
    in
        case model.route of
            Top ->
                div [  ]
                    [ h1 [ class "title message-header" ] [ text "平均値、変動係数、標準偏差、偏差" ]
                        , inputView "下の入力欄に数字を入れてね" ", 間隔で数字を入力してね。" model.listOne OneList
                        , Html.table [ class "table is-bordered" ] 
                            [ Html.tr [] 
                                [ oneValueView model.listOne "入力された値の平均値は：" Stat.average
                                , manyValueView model.listOne "入力された値の偏差は：" Stat.deviation
                                , oneValueView model.listOne "入力された値の標準偏差は：" Stat.standardDeviation
                                , oneValueView model.listOne "入力された値の変動係数は：" Stat.coefficientOfVariation
                                , manyValueView model.listOne "入力された値を標準化すると：" Stat.standartdization
                                , div [] [ text "標準化したリストをグラフで示すと"
                                        , Chart.listVisualizeArgOne (model.listOne |> stringToListFloat |> Stat.standartdization)
                                        ]
                                , manyValueView model.listOne "入力された値のシャープレシオを示した数値は：" Stat.shapeRetioList
                                , fiducialView (Stat.fiducialInterval (toFloat (List.length (one) )) (Stat.standardDeviation (one)))
                                ]
                            ]
                    ]

            Ols ->
                div [  ]
                [ h1 [ class "title message-header" ] [ text "最小二乗法" ]
                , inputView "リストその１" ", 間隔で数字を入力してね。" model.listOne OneList
                , inputView "リストその２" ", 間隔で数字を入力してね。" model.listTwo TwoList
                , inputView "リストその３" ", 間隔で数字を入力してね。" model.listThree ThreeList
                , br [][]
                , olsRawDataView one two
                , br [][]
                , olsClassifiedDataView one two three
                ]

            Regres ->
                div [  ]
                [ h1 [ class "title message-header" ] [ text "回帰分析" ]
                , inputView "リストその１" ", 間隔で数字を入力してね。" model.listOne OneList
                , inputView "リストその２" ", 間隔で数字を入力してね。" model.listTwo TwoList
                , regressionView one two three
                ]

            Dist ->
                div [  ]
                [ h1 [ class "title message-header" ] [text "二項分布・ポアソン分布" ]
                , inputView "リストその１" ", 間隔で数字を入力してね。全部合計されるよ" model.listOne OneList
                , div [] 
                    [ h2 [ class "subtitle" ] [ text "二項分布をしてみたら" ]
                    , p [] [ text "リストで表示すると以下のとおりになる" ]
                    , p [] [ text ( Stat.biDistribution (List.sum one) (List.length one) |> listFloatToString ) ]
                    ]
                , div [] 
                    [ h2 [ class "subtitle" ] [text "ポアソン分布をしてみたら" ]
                    , p [] [ text "リストで表示すると以下のとおりになる" ]
                    , p [] [ text ( Stat.poisson (List.sum one) (List.length one) |> listFloatToString ) ]
                    ]
                ]

            Parcen ->
                div [  ]
                [ h1 [ class "title message-header" ] [text "確率計算" ]
                , inputView "どれくらいものがあるのか" ", 間隔で数字を入力してね。全部合計されるよ" model.listOne OneList
                , inputView "その中からいくつ持ってくのか" ", 間隔で数字を入力してね。上の数からどれくらい持っていく？" model.listTwo TwoList
                , parcentageView oneInt twoInt
                ]

            Hypo ->
                div [  ]
                [ h1 [ class "title message-header" ] [text "仮説検定" ]
                , inputView "仮説検定するサンプルを入力してください" "これをもとに仮説検定していきます" model.listOne OneList
                , inputView "仮説検定する値を入力してください" "入力した値とサンプルを比較して正しいかどうか調べます。" model.listTwo TwoList
                , hypoView one (List.sum two)
                ]


buttonLink: Msg -> String -> Html Msg
buttonLink msg str =
    li [ class "" ] [ a [ class "panel-block button is-link is-outlined is-fullwidth", onClick msg ] [ text str ] ]


inputView: String -> String -> String -> (String -> Msg) -> Html Msg
inputView pText inputValue model msg =
    div [] [ p [] [ text pText ]
           , input [ class "input", placeholder inputValue, value model, onInput msg ] []
        ]


{-|
List Float -> Float を受け取って
listFloatView model.string "何らかのメッセージ" average
-}
oneValueView: String -> String -> (List Float -> Float) -> Html Msg
oneValueView listFloat strText funcL =
    div [ class "" ] [  Html.td [ ] [ text <| strText ], Html.td [] [ text <| String.fromFloat <| roundNum 4 <| funcL <| stringToListFloat listFloat ] ]


{-| listView
-}
manyValueView: String -> String -> (List Float -> List Float) -> Html Msg
manyValueView listFloat strText funcL =
    div [] [ Html.td [] [text <| strText], Html.td [] [ text <| listFloatToString <| List.map (\x -> roundNum 4 x) <| funcL <| stringToListFloat listFloat ] ]


fiducialView: Dict String Float -> Html Msg
fiducialView fiducialDict =
  let
      mini = (String.fromFloat <| roundNum 4 <| Maybe.withDefault 0 (Dict.get "min" fiducialDict))
      maxi = (String.fromFloat <| roundNum 4 <| Maybe.withDefault 0 (Dict.get "max" fiducialDict))
  in
    div [] [ Html.td [] [ text "信頼区間の...", text ("最小値は" ++ mini), text ("最大値は" ++ maxi) ]   ]


olsRawDataView: List Float -> List Float -> Html Msg
olsRawDataView xi yi =
    let
        ols = Stat.olsRawData xi yi
        b = dictInFloatToString ols "b"
        a = dictInFloatToString ols "a"
        r2 = dictInFloatToString ols "r2"
        r = dictInFloatToString ols "r"
    in
        div []
            [ h2 [ class "subtitle" ] [ text "Raw data の場合の最小2乗法" ]
            , ul []
                [ li [] [ text ("勾配 = " ++ (String.fromFloat b) ) ]
                , li [] [ text ("定数項 = " ++ (String.fromFloat a) )]
                , li [] [ text ("決定係数 = " ++ (String.fromFloat r2 )) ]
                , li [] [ text ("相関係数 = " ++ (String.fromFloat r )) ]
                ]
            ]


olsClassifiedDataView: List Float -> List Float -> List Float -> Html Msg
olsClassifiedDataView xi yi f =
    let
        ols = Stat.olsClassifiedData xi yi f
        b = dictInFloatToString ols "b"
        a =  dictInFloatToString ols "a"
        r2 = dictInFloatToString ols "r2"
        r = dictInFloatToString ols "r"
    in
        div []
            [ h2 [ class "subtitle" ] [ text "Classified data の場合の最小2乗法" ]
            , ul []
                [ li [] [ text ("勾配 = " ++ (String.fromFloat b) ) ]
                , li [] [ text ("定数項 = " ++ (String.fromFloat a) )]
                , li [] [ text ("決定係数 = " ++ (String.fromFloat r2 ) ) ]
                , li [] [ text ("相関係数 = " ++ (String.fromFloat r ) ) ]
                ]
            ]

regressionView: List Float ->  List Float ->  List Float -> Html Msg
regressionView xi yi _ =
  let
      regression = Stat.regressionAnalysisRaw xi yi
      b = dictInFloatToString regression "b"
      a = dictInFloatToString regression "a"
      r = dictInFloatToString regression "r"
      ta = dictInFloatToString regression "ta"
      tb = dictInFloatToString regression "tb"
  in
        div []
            [ h2 [ class "subtitle" ] [ text "回帰分析した結果↓" ]
            , ul [ class "" ]
                [ li [ class "" ] [ text ("勾配 = " ++ String.fromFloat b ) ]
                , li [ class "" ] [ text ("定数項 = " ++ String.fromFloat a )]
                , li [ class "" ] [ text ("相関係数 = " ++ String.fromFloat r ) ]
                , li [ class "" ] [ text ("回帰係数a = " ++ String.fromFloat ta ) ]
                , li [ class "" ] [ text ("回帰係数b = " ++ String.fromFloat tb ) ]
                ]
            ]


parcentageView: Int -> Int -> Html Msg
parcentageView oneInt twoInt =
    div []
    [ ul []
        [ li [] [ text ("リストの合計" ++ String.fromInt oneInt) ]
        , li [] [ text ("階乗計算じゃよ" ++ String.fromInt (Utility.factorial oneInt)) ]
        , li [] [ text ("順列なのだよ" ++ String.fromInt (Utility.permutation oneInt twoInt)) ]
        , li [] [ text ("組み合わせです" ++ String.fromInt (Utility.combination oneInt twoInt)) ]
        ]
    ]


hypoView: List Float -> Float -> Html Msg
hypoView listFloat sample = 
    let
        sCount = List.length listFloat
        sAverage = Stat.average listFloat
        sDeviation = Stat.standardDeviation listFloat
        notAlpha = Stat.hypothesisNotAlpha sAverage sCount sample sDeviation 
    in
        div []
        [ h2 [] [ text "この仮説は有効であるか否か・・・" ]
        , ul []
            [ li [] [ text ("標本平均は" ++ String.fromFloat sAverage) ]
            , li [] [ text ("サンプルの数は" ++ String.fromInt sCount) ]
            ]
        , p [] [ text ("仮説として登場した数" ++ String.fromFloat sample ++ "は棄却域なのか") ]
        ,  if notAlpha == True then
            div []
            [ p [] [ text ("帰無仮説として登場した μ = " ++ String.fromFloat sample ++ "は棄却されないため") ]
            , p [] [ text ("対立仮説として登場した μ > " ++ String.fromFloat sample ++ "は採択される。")]
            , p [] [ text "つまり棄却できない。"]
            ]
         else
            div []
            [ p [] [ text ("帰無仮説として登場した μ = " ++ String.fromFloat sample ++ "は棄却できないので") ]
            , p [] [ text ("対立仮説として登場した μ > " ++ String.fromFloat sample ++ "は採択されない。")]
            , p [] [ text "採択できない。"]
            ]
        ]