module Receive exposing (..)

import Json.Decode as D
import Html exposing (..)
import Dict exposing (Dict)
import Csv


type HttpState
    = Fail
    | Wait
    | Success


type DictState
    = Failue
    | Waitin
    | SuccessJoin


{-
受け取るデータの色々
-}
type alias DualList =
    { listOne : String
    , listTwo : String
    }


type alias Dist =
    {}


listDecoder : D.Decoder DualList
listDecoder =
    D.map2 DualList
        (D.field "listOne" D.string)
        (D.field "listTwo" D.string)


selectDictDecoder : D.Decoder SelectDict
selectDictDecoder =
    Csv.Decode.


{-
ファイルをアップロードしてそのデータをもとに加工されたデータを受け取りたい。
つまりアップデートするさいに行う処理の中にデータを受け取ることが考えられる。
タプルで受け取ろうにもfieldの問題でいい感じに受け取れない。
まず思ったのはHTTP関連は１つのファイルでなんとかしたほうがいいということ
-}