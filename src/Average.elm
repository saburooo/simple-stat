module Average exposing (..)

import Stat
import Html exposing (..)
import Html.Attributes exposing (..)


type alias Model =
    { state : State
    , listOne : String
    , listTwo : String
    , listThee : String
    }


type State
    = Init
    | Input String


init : ( Model, Cmd Msg )
init =
    ( Model Init "" "" "", Cmd.none )


-- UPDATE


type Msg
    = OneList String
    | TwoList String
    | ThreeList String


update : Msg -> Model ->  ( Model, Cmd Msg )
update msg model =
    case msg of
        OneList str ->
             ( { model | listOne=str }, Cmd.none )

        TwoList str ->
             ( { model | listTwo=str }, Cmd.none )

        ThreeList str ->
             ( { model | listThee=str }, Cmd.none )


view : Model -> Html Msg
view model =
    case model.state of
        Init ->
            text "Loading..."

        Input _ ->
            Debug.todo "どうやって平均を得る"

