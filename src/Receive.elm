module Receive exposing (..)

import Json.Decode as D
import Http
import Html exposing (..)
import List


type Msg
    = GotStrOne String
    | GotStrTwo String


getApiList : Cmd Msg
getApiList =
    Http.get
        { url = "http://127.0.0.1:5000/api"
        , expect = Http.expectString 
        }