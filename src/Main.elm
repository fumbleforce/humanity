module Main exposing (..)

import Model exposing (init)
import View exposing (view)
import Update exposing (update)
import Subs exposing (subscriptions)

main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

