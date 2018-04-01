module Update exposing (..)

import Random exposing (initialSeed)

import Types exposing (..)
import Model exposing (Model, encodeModel)
import Game.Update as Game
import Population.Update as Population

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    (mainModel, mainMsg) = updateMain msg model
    (gameModel, gameMsg) = Game.update msg model
    (popModel, popMsg) = Population.update msg gameModel
  in
    (popModel, Cmd.batch [gameMsg, popMsg])


updateMain : Msg -> Model -> (Model, Cmd Msg)
updateMain msg model =
  case msg of
    OnTime time ->
      ({ model | seed = initialSeed <| round time }, Cmd.none)

    _ ->
      (model, Cmd.none)
