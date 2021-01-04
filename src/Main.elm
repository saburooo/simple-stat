{-
メインモジュールのゴールはどうするか検討中
クリックするたび計算式が変わるなんてのはどうだろうか
その場合、入力した値を小数点入りの数値のリストとして受け付ける必要がある、
モデルは如何定義するかはまだ検討中
-}

module Main exposing (..)


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
  = status | Status
