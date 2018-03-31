module Update exposing (..)

import Types exposing (..)
import Model exposing (Model)
import Game.Update as Game
import Population.Update as Population


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    (gameModel, gameMsg) = Game.update msg model
    (popModel, popMsg) = Population.update msg gameModel
  in
    (popModel, Cmd.batch [gameMsg, popMsg])
