module Subs exposing (subscriptions)

import Keyboard
import Window
import AnimationFrame

import Model exposing (Model)
import Types exposing (Msg(SpaceDown, Tick, NoOp, WindowSize))

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

