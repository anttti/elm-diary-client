module Main exposing (main)

import Diary
import Html


main : Program Never Diary.Model Diary.Msg
main =
    Html.program
        { view = Diary.view
        , update = Diary.update
        , init = Diary.init
        , subscriptions = \_ -> Sub.none
        }
