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
import Browser

import Round exposing (roundNum)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

import Dict exposing (Dict)
import Html.Events exposing (onClick)


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


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
      Sub.none


-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "オレオレ統計ライブラリーデモ"
    , body =
        -- 一番上にリンクを貼ってなんとかする。
        [
            div [ class "columns" ]
            [
                div [ class "column is-one-thirds" ] 
                [ h1 [ class "title"] [ text "メニューです" ]
                    , ul [ class "" ]
                        [ buttonLink TopPage "平均値"
                        , buttonLink OLS "OLS最小二乗法"
                        , buttonLink Regression "回帰分析"
                    ]
                ]
                , div [ class "column is-two-thirds" ]
                    [ topView model
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
    in
        case model.route of
            Top ->
                div [] [ h1 [ class "title" ] [ text "平均値、変動係数、標準偏差、偏差" ]
                        , input [ class "input", placeholder ", 間隔で数字を入力してね。", value model.listOne, onInput OneList ] []
                        , br [] []
                        , oneValueView model.listOne "入力された値の平均値は" Stat.average
                        , oneValueView model.listOne "入力された値の変動係数は" Stat.coefficientOfVariation
                        , oneValueView model.listOne "入力された値の標準偏差は" Stat.standardDeviation
                        , manyValueView model.listOne "入力された値の偏差は" Stat.deviation
                        , fiducialView (Stat.fiducialInterval (toFloat (List.length (one) )) (Stat.standardDeviation (one)))
                    ]

            Ols ->
                div []
                [ h1 [ class "title" ] [ text "工事中" ]
                , p [] [ text "リストその１" ]
                , input [ class "input", placeholder ", 間隔で数字を入力してね。", value model.listOne, onInput OneList ] []
                , p [] [ text "リストその２" ]
                , input [ class "input", placeholder ", 間隔で数字を入力してね。", value model.listTwo, onInput TwoList ] []
                , p [] [ text "リストその３" ]
                , input [ class "input", placeholder ", 間隔で数字を入力してね。", value model.listThree, onInput ThreeList ] []
                , div [] [ text "Raw data の推計" ]
                , div [] [ text "Classified data の推計" ]
                ]

            Regres ->
                Debug.todo "回帰分析専用ページを設ける。"


{-|
List Float -> Float を受け取って
listFloatView model.string "何らかのメッセージ" average
-}
oneValueView: String -> String -> (List Float -> Float) -> Html Msg
oneValueView listFloat strText funcL =
    div [ class "" ] [ text <| String.append strText <| String.fromFloat <| roundNum 4 <| funcL <| stringToListFloat listFloat ]


{-| listView
-}
manyValueView: String -> String -> (List Float -> List Float) -> Html Msg
manyValueView listFloat strText funcL =
    div [] [ text <| String.append strText <| listFloatToString <| List.map (\x -> roundNum 4 x) <| funcL <| stringToListFloat listFloat ]


fiducialView: Dict String Float -> Html Msg
fiducialView fiducialDict =
  let
      mini = (String.fromFloat <| roundNum 4 <| Maybe.withDefault 0 (Dict.get "min" fiducialDict))
      maxi = (String.fromFloat <| roundNum 4 <| Maybe.withDefault 0 (Dict.get "max" fiducialDict))
  in
    div [] [ text ("信頼区間の...最小値は" ++ mini ++ "で最大値は" ++ maxi) ]


buttonLink: Msg -> String -> Html Msg
buttonLink msg str =
    li [] [ a [ class "button is-link is-outlined is-small is-fullwidth", onClick msg ] [ text str ] ]
