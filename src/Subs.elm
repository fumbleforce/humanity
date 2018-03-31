module Subs exposing (..)

import Keyboard
import Task
import Html
import Window
import AnimationFrame

import Model exposing (Model)
import Game.Subs as Game

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Game.subscriptions model
    ]
