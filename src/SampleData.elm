module SampleData exposing (..)
import MainUtility exposing (stringToListFloat)

{-| 2020sukeijinkou.csv、新潟市、クリエイティブ・コモンズ・ライセンス 表示 2.1 日本(https://creativecommons.org/licenses/by/2.1/jp/)
2019sukeijinkou.csv、新潟市、クリエイティブ・コモンズ・ライセンス 表示 2.1 日本(https://creativecommons.org/licenses/by/2.1/jp/)
2018sukeijinkou.csv、新潟市、クリエイティブ・コモンズ・ライセンス 表示 2.1 日本(https://creativecommons.org/licenses/by/2.1/jp/)
https://creativecommons.org/licenses/by/2.1/jp/
-}


niigataData : String
niigataData =
    "807136,806701,806113,803401,805098,805198,804809,804594,804308,804152,804242,804130,803802,803411,802887,801298,801806,801591,801249,801034,800882,800582,800515,800273,800000,799565,798840,797029,797735,797591,797313,797160,796770,796500,796259,795983,795597,795185,794649,793138,793757,793586,793489,793383,793113,792887,792520,792258"

niigata2017 : String
niigata2017 =
        "807136,806701,806113,803401,805098,805198,804809,804594,804308,804152,804242,804130"


niigata2018 : String
niigata2018 =
        "803802,803411,802887,801298,801806,801591,801249,801034,800882,800582,800515,800273"


niigata2019 : String
niigata2019 =
    "800000,799565,798840,797029,797735,797591,797313,797160,796770,796500,796259,795983"


niigata2020 : String
niigata2020 = "795597,795185,794649,793138,793757,793586,793489,793383,793113,792887,792520,792258"


niigata2017ToFloat : List Float
niigata2017ToFloat =
    stringToListFloat niigata2017


niigata2018ToFloat : List Float
niigata2018ToFloat =
    stringToListFloat niigata2018


niigata2019ToFloat : List Float
niigata2019ToFloat =
    stringToListFloat niigata2019


niigata2020ToFloat : List Float
niigata2020ToFloat =
    stringToListFloat niigata2020

