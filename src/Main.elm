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

import Stat as Stat


-- MAIN
main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


init : Model
init =
  []


-- MODEL
type Status
  = Avarage
  | Deviation
  | StandardDeviation


type alias Model
  = List



type alias Msg
  = {status | Status}
