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

import Url
import Url exposing (Url)

import Url.Parser exposing ((</>), s, top)
import Url.Parser exposing (Parser)
import Url.Parser exposing (oneOf)
import Url.Parser

import Dict exposing (Dict)

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



init : () -> ( Model, Cmd Msg)
init _  =
    (Model "" "" "" Top, Cmd.none)


-- UPDATE


type Msg
    = OneList String
    | TwoList String
    | ThreeList String


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


{-|
  stringToListFloat "1,2,3,4,5"
  OUT [ 1.0, 2.0, 3.0, 4.0, 5.0 ]
-}
stringToListFloat: String -> List Float
stringToListFloat str =
    let
       strList = String.split ", " str
    in
      List.map (\s -> Maybe.withDefault 0 (String.toFloat s)) strList
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
        [ h1 [] [ text "この中から計算して欲しいものを選んでね:" ]
        , br [] []
        , input [ placeholder "何らかの数字を , 間隔で入力してね。", value model.listOne, onInput OneList ] []
        , input [ placeholder "何らかの数字を , 間隔で入力してね。", value model.listTwo, onInput TwoList ] []
        , input [ placeholder "何らかの数字を , 間隔で入力してね。", value model.listThree, onInput ThreeList ] []
        , oneValueView model.listOne "入力された値の平均値:" Stat.average
        , oneValueView model.listTwo "入力された値の変動係数:" Stat.coefficientOfVariation
        , oneValueView model.listOne "入力された値の標準偏差:" Stat.standardDeviation
        , manyValueView model.listOne "入力された値の偏差:" Stat.deviation
        , fiducialView (Stat.fiducialInterval (toFloat (List.length (stringToListFloat model.listOne) )) (Stat.standardDeviation (stringToListFloat model.listOne)))
        ]
    }


{-|
List Float -> Float を受け取って
listFloatView model.string "何らかのメッセージ" average
-}
oneValueView: String -> String -> (List Float -> Float) -> Html Msg
oneValueView listFloat strText funcL =
    div [] [ text <| String.append strText <| String.fromFloat <| roundNum 4 <| funcL <| stringToListFloat listFloat ]


{-| listView
-}
manyValueView: String -> String -> (List Float -> List Float) -> Html Msg
manyValueView listFloat strText funcL =
    div [] [ text <| String.append strText <| listFloatToString <| List.map (\x -> roundNum 4 x) <| funcL <| stringToListFloat listFloat ]


fiducialView: Dict String Float -> Html Msg
fiducialView fiducialDict =
  let
      mini = (String.fromFloat <| Maybe.withDefault 0 (Dict.get "min" fiducialDict))
      maxi = (String.fromFloat <| Maybe.withDefault 0 (Dict.get "max" fiducialDict))
  in
    div [] [ text ("信頼区間の...最小値" ++ mini ++ " 最大値" ++ maxi) ]
