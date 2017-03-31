module Main exposing (..)

--import Json.Decode as D exposing (field)
import Keyboard
import Task
import Html
import Window
import AnimationFrame

import Game exposing (Model, State(..), Msg(..))
import View
import Update
import People.People

main : Program Never Model Msg
main =
  Html.program
    { init = initModelAndCommands
    , view = View.view
    , update = Update.update
    , subscriptions = subscriptions
    }


initModelAndCommands : ( Model, Cmd Msg )
initModelAndCommands =
  ( defaultModel, getWindowSizeCommand )


getWindowSizeCommand : Cmd Msg
getWindowSizeCommand =
  Task.perform WindowSize Window.size


defaultModel : Model
defaultModel =
  { state = Stopped
  , windowDimensions = { width = 640, height = 480 }
  , day = 0
  , year = 0
  , people = People.People.initialPeople
  , selectedPerson = Nothing
  }


keyDownToMsg : Keyboard.KeyCode -> Msg
keyDownToMsg kc =
  case kc of
    32 ->
      SpaceDown
    _ ->
        NoOp


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Keyboard.downs keyDownToMsg
    , AnimationFrame.diffs (\dt -> Tick (dt / 1000))
    , Window.resizes WindowSize
    ]
