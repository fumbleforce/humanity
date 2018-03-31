module Game.Subs exposing (..)

import Keyboard
import Task
import Html
import Window
import AnimationFrame

import Game.Msg exposing (Msg(..))
import Game.Model exposing (Model)


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Keyboard.downs keyDownToMsg
    , AnimationFrame.diffs (\dt -> Tick (dt / 1000))
    , Window.resizes WindowSize
    ]



keyDownToMsg : Keyboard.KeyCode -> Msg
keyDownToMsg kc =
  case kc of
    32 ->
      SpaceDown
    _ ->
        NoOp

