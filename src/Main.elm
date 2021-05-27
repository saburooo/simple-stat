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

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

import List

import Stat
import Chart
import Utility
import SampleData
import MainUtility exposing (..)

import Browser

import Round exposing (roundNum)
import Stat exposing (MinMax)


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
    , calcurate : Bool
    , niigata : Bool
    }


-- URL
type Route
    = How
    | Top
    | Ols
    | Regres
    | Dist
    | Parcen
    | Hypo


init : () -> ( Model, Cmd Msg)
init _  =
    (Model "" "" "" Top False False, Cmd.none)


-- UPDATE


type Msg
    = OneList String
    | TwoList String
    | ThreeList String
    | HowToUse
    | TopPage
    | OLS
    | Regression
    | Distribution
    | Parcentage
    | Hypothesis
    | ClickCalc
    | Niigata


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

        HowToUse ->
            ( { model | route = How }
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

        ClickCalc ->
            if model.calcurate == True then
                ( { model | calcurate = False }
                , Cmd.none
                )
            else
                ( { model | calcurate = True }
                , Cmd.none
                )
        
        -- 新潟県の人口推移を計算する
        Niigata ->
            if model.niigata == True then
                ( { model | niigata = False }
                , Cmd.none
                )
            else
                ( { model | niigata = True }
                , Cmd.none
                )


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
                            [ buttonLink HowToUse "使い方"
                            , buttonLink TopPage "平均値"
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
        oneInt = strToIntList model.listOne
        twoInt = strToIntList model.listTwo
    in
        case model.route of
            How ->
                div []
                    [ h1 [ class "title message-header" ] [ text "このアプリの使い方" ]
                    , p [ ] [ text "入力欄に,の間隔で半角の数字を入力してね" ]
                    , p [ ] [ text "例：1,2,3,4,5" ]
                    , p [ ] [ text "一部外部サイトのデータを使用しています。"]
                    , p [ ] [ text "2020sukeijinkou.csv、新潟市、クリエイティブ・コモンズ・ライセンス 表示 2.1 日本" ]
                    , p [ ] [ text "2019sukeijinkou.csv、新潟市、クリエイティブ・コモンズ・ライセンス 表示 2.1 日本" ]
                    , p [ ] [ text "2018sukeijinkou.csv、新潟市、クリエイティブ・コモンズ・ライセンス 表示 2.1 日本" ]
                    , a [ href "https://creativecommons.org/licenses/by/2.1/jp/" ] [ text "https://creativecommons.org/licenses/by/2.1/jp/（外部サイト）"]
                    ]

            Top ->
                div [  ]
                    [ h1 [ class "title message-header" ] [ text "平均値、変動係数、標準偏差、偏差" ]
                        , inputView "下の入力欄に数字を入れてね" ", 間隔で数字を入力してね。" model.listOne OneList
                        , calcButtonView
                        , niigataButtonView model
                        , if model.calcurate == True then
                            Html.table [ class "table is-bordered" ]
                                [ if model.niigata == True then
                                    Html.tr [] 
                                        [ p [ ] [ text "新潟県の2018年から2020年までの１ヶ月ごとの総人口から計算" ]
                                        , oneValueView SampleData.niigataData "入力された値の平均値は：" Stat.average
                                        , manyValueView SampleData.niigataData "入力された値の偏差は右の値、グラフにすると以下" Stat.deviation
                                        , Chart.listGraph ( List.map (\o -> abs o ) ( Stat.deviation ( stringToListFloat SampleData.niigataData ) ) )
                                        , Chart.listCircle ( List.map (\o -> abs o ) ( Stat.deviation ( stringToListFloat SampleData.niigataData ) ) )
                                        , oneValueView SampleData.niigataData "入力された値の標準偏差は：" Stat.standardDeviation
                                        , oneValueView SampleData.niigataData "入力された値の変動係数は：" Stat.coefficientOfVariation
                                        , oneValueView SampleData.niigataData "入力された値の中央値は：" Utility.median
                                        , manyValueView SampleData.niigataData "入力された値を標準化すると：" Stat.standartdization
                                        , fiducialView (Stat.fiducialInterval (toFloat (List.length (( stringToListFloat SampleData.niigataData )) )) (Stat.standardDeviation (( stringToListFloat SampleData.niigataData ))))
                                        ]
                                else
                                    Html.tr [] 
                                        [ oneValueView model.listOne "入力された値の平均値は：" Stat.average
                                        , manyValueView model.listOne "入力された値の偏差は右の値、グラフにすると以下" Stat.deviation
                                        , Chart.listGraph ( List.map (\o -> abs o ) ( Stat.deviation one ) )
                                        , Chart.listCircle ( List.map (\o -> abs o ) ( Stat.deviation one ) )
                                        , oneValueView model.listOne "入力された値の標準偏差は：" Stat.standardDeviation
                                        , oneValueView model.listOne "入力された値の変動係数は：" Stat.coefficientOfVariation
                                        , oneValueView model.listOne "入力された値の中央値は：" Utility.median
                                        , manyValueView model.listOne "入力された値を標準化すると：" Stat.standartdization
                                        , fiducialView (Stat.fiducialInterval (toFloat (List.length (one) )) (Stat.standardDeviation (one)))
                                        ]
                                ]
                        else
                            p [] [ text "計算する待ちです。" ]
                        , h2 [ class "subtitle is-4" ] [ text "解説" ]
                        , p [] [ text "平均値とは全体のデータを受け取って真ん中の値を返す計算式のことでこのアプリで求められる平均は「算術平均」と呼ばれる。" ]
                        , p [] [ text "このように求められた値は別名代表値という" ]
                        , p [] [ text "代表値には様々な求め方があり、同じ平均でも幾何平均、調和平均等がありそれぞれ相応しい用い方を求められる。" ]
                    ]

            Ols ->
                div [  ]
                [ h1 [ class "title message-header" ] [ text "最小二乗法" ]
                , inputView "リストその１" ", 間隔で数字を入力してね。" model.listOne OneList
                , inputView "リストその２" ", 間隔で数字を入力してね。" model.listTwo TwoList
                , inputView "リストその３" ", 間隔で数字を入力してね。" model.listThree ThreeList
                , calcButtonView
                , niigataButtonView model
                , if model.calcurate == True then
                    if model.niigata == True then
                        div []
                            [ p [ ] [ text "取得したデータ" ]
                            [ p [ ] [ text "新潟県の2018年時点での月間人口:" ++ SampleData.niigata2018 ]
                            [ p [ ] [ text "新潟県の2019年時点での月間人口:" ++ SampleData.niigata2019 ]
                            [ p [ ] [ text "新潟県の2020年時点での月間人口:" ++ SampleData.niigata2020 ]
                            , olsRawDataView SampleData.niigata2018ToFloat SampleData.niigata2019ToFloat
                            , olsClassifiedDataView SampleData.niigata2018ToFloat SampleData.niigata2019ToFloat SampleData.niigata2020ToFloat
                            ]
                    else
                        div []
                            [ olsRawDataView one two
                            , olsClassifiedDataView one two three
                            ]
                else
                    p [] [ text "ボタンを押してね" ]
                ]

            Regres ->
                div [  ]
                [ h1 [ class "title message-header" ] [ text "回帰分析" ]
                , inputView "リストその１" ", 間隔で数字を入力してね。" model.listOne OneList
                , inputView "リストその２" ", 間隔で数字を入力してね。" model.listTwo TwoList
                , niigataButtonView model
                , if model.calcurate == True then
                    if model.niigata == True then
                        div [] 
                            [ regressionView SampleData.niigata2018ToFloat SampleData.niigata2019ToFloat SampleData.niigata2020ToFloat
                            ]
                    else
                        div [] 
                            [ regressionView one two three
                            ]
                else
                    p [] [ text "ボタンを押してね" ]
                ]

            Dist ->
                div [  ]
                [ h1 [ class "title message-header" ] [text "二項分布・ポアソン分布" ]
                , inputView "リストその１ - 確率を入力してね" ", 間隔で数字を入力してね。全部合計されるよ" model.listOne OneList
                , inputView "リストその２ - 回数を入力してね" ", 間隔で数字を入力してね。全部合計されるよ" model.listTwo TwoList
                , p [] [ text ("リストその1は" ++ String.fromFloat (List.sum one) ++ "リストその2は" ++ String.fromInt (List.sum twoInt) )]
                , calcButtonView
                , niigataButtonView model
                , if model.calcurate == True then
                    div [] 
                        [ distriView model one twoInt
                        ]
                else
                    p [] [ text "ボタンを押してね" ]
                ]

            Parcen ->
                div [  ]
                [ h1 [ class "title message-header" ] [text "確率計算" ]
                , inputView "どれくらいものがあるのか" ", 間隔で数字を入力してね。全部合計されるよ" model.listOne OneList
                , inputView "その中からいくつ持ってくのか" ", 間隔で数字を入力してね。上の数からどれくらい持っていく？" model.listTwo TwoList
                , calcButtonView
                , niigataButtonView model
                , if model.calcurate == True then
                    div [] 
                        [ parcentageView oneInt twoInt
                        ] 
                else
                    p [] [ text "ボタンを押してね" ]
                ]

            Hypo ->
                div [  ]
                [ h1 [ class "title message-header" ] [text "仮説検定" ]
                , inputView "仮説検定するサンプルを入力してください" "これをもとに仮説検定していきます" model.listOne OneList
                , inputView "仮説検定する値を入力してください" "入力した値とサンプルを比較して正しいかどうか調べます。" model.listTwo TwoList
                , calcButtonView
                , niigataButtonView model
                , if model.calcurate == True then
                    div [] 
                    [ hypoView one (List.sum two) ]
                else
                    p [] [ text "ボタンを押してね" ]
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
    div [ class "" ] [  Html.td [ ] [ text <| strText ], Html.td [] [ text <| String.fromFloat <| roundNum 4 <| funcL <| stringToListFloat listFloat ]
    ]


{-| listView
-}
manyValueView: String -> String -> (List Float -> List Float) -> Html Msg
manyValueView listFloat strText funcL =
    div [] [ Html.td [] [text <| strText], Html.td [] [ text <| listFloatToString <| List.map (\x -> roundNum 4 x) <| funcL <| stringToListFloat listFloat ] ]


fiducialView: MinMax -> Html Msg
fiducialView fiducial =
  let
      mini = (String.fromFloat <| roundNum 4 <| fiducial.min)
      maxi = (String.fromFloat <| roundNum 4 <| fiducial.max)
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


parcentageView: List Int -> List Int -> Html Msg
parcentageView oneInt twoInt =
    let
        sumOne = List.sum oneInt
        sumTwo = List.sum twoInt
    in
    div []
    [ ul []
        [ li [] [ text ("リストの合計" ++ String.fromInt sumOne) ]
        , li [] [ text ("階乗計算じゃよ" ++ String.fromInt (Utility.factorial sumOne )) ]
        , li [] [ text ("順列だよ" ++ String.fromInt (Utility.permutation sumOne sumTwo )) ]
        , li [] [ text ("組み合わせです" ++ String.fromInt (Utility.combination sumOne sumTwo )) ]
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

distriView: Model -> List Float -> List Int -> Html Msg
distriView b one twoInt =
    if b.calcurate == True then
        div [ class "content" ] 
            [ h3 [ class "subtitle" ] [ text "二項分布をしてみたら" ]
            , p [] [ text "リストで表示すると以下のとおりになる" ]
            , p [] [ text ( Stat.biDistribution (List.sum one) (List.sum twoInt) |> listFloatToString ) ]
            , h3 [ class "subtitle" ] [text "ポアソン分布をしてみたら" ]
            , p [] [ text "リストで表示すると以下のとおりになる" ]
            , p [] [ text ( Stat.poisson (List.sum one) (List.sum twoInt) |> listFloatToString ) ]
            , h3 [ class "subtitle" ] [ text "二項分布をグラフっぽくすると" ]
            , Chart.listVisualizeArgOne ( Stat.biDistribution (List.sum one) (List.sum twoInt) ) 
            , h3 [ class "subtitle" ] [ text "ポアソン分布をグラフっぽくすると" ]
            , Chart.listVisualizeArgOne ( Stat.poisson (List.sum one) (List.sum twoInt) ) 
            ]
    else
        div [] [ text "クリックすると計算されるよ。" ]


calcButtonView : Html Msg
calcButtonView =
    div [ class "level-right" ] 
        [ button [ class "button is-large is-info", onClick ClickCalc ] [ text "計算する" ] ]


niigataButtonView : Model -> Html Msg
niigataButtonView model =
    div [ class "level-left" ]
        [ if model.niigata == True then
                button [ class "button is-middle is-info", onClick Niigata ] [ text "計算するをクリックしてみてね" ]
            else
                button [ class "button is-middle is-info", onClick Niigata ] [ text "新潟県の年ごとの人口を計算する。" ]
        ]
