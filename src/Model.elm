module Model exposing (..)

import Task

import Msg exposing (Msg)
import Game.Model as Game
import Population.Model as Population

type alias Model =
  { game : Game.Model
  , population: Population.Model
  }


init : ( Model, Cmd Msg )
init =
  ( initialModel, getWindowSizeCommand )


initialModel : Model
initialModel =
  { game = Game.initialModel
  , population = Population.initialModel
  }


getWindowSizeCommand : Cmd Msg
getWindowSizeCommand =
  Task.perform WindowSize Window.size


