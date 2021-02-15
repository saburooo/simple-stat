# simple-stat

simple-stat は私自身がelmと統計の勉強のために自作したライブラリです。

そのため間違っているところがあったらツッコミ大歓迎です。

# En

simple-stat is a library that I made myself to study elm and statistics.

If you find any mistakes in it, please feel free to correct me.

# Sample

URL:[https://mysimple-stat.web.app](https://mysimple-stat.web.app)

# Example

```elm
import Stat


type alias Model =
    { listOne : String }


listFloatToString:List Float -> String
listFloatToString listFloat =
    let
        strChanged = List.map (\el -> String.fromFloat el) listFloat
    in
        String.join ", " strChanged


stringToListFloat: String -> List Float
stringToListFloat str =
    let
       strList = String.split "," str
    in
      List.map (\s -> Maybe.withDefault 0 (String.toFloat s)) strList


manyValueView: String -> String -> (List Float -> List Float) -> Html Msg
manyValueView listFloat strText funcL =
    div [] [ Html.td [] [text <| strText], Html.td [] [ text <| listFloatToString <| List.map (\x -> roundNum 4 x) <| funcL <| stringToListFloat listFloat ] ]


view : Model -> Html Msg
view model =
    div [] [
        manyValueView model.listOne "The deviation of the entered value is:" Stat.deviation
        ]
```

# Author

saburooo

# License

"simple-stat" is under [MIT license](https://en.wikipedia.org/wiki/MIT_License).
