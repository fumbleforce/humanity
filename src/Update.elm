module Update exposing (update)

import Random exposing (initialSeed)
import List exposing (foldl)
import Types exposing (Msg(OnTime, SelectTab))
import Model exposing (Model, encodeModel)
import Game.Update as Game
import Population.Update as Population

type alias UpdateFunction = Msg -> Model -> (Model, Cmd Msg)

mergeUpdates:
  Msg
  -> UpdateFunction
  -> (Model, List (Cmd Msg))
  -> (Model, List (Cmd Msg))
mergeUpdates msg updater (model, msgs) =
  let
    (newModel, newMsg) = updater msg model
  in
    (newModel, newMsg :: msgs)

update : UpdateFunction
update msg model =
  let
    (newModel, msgs) = foldl
      (mergeUpdates msg)
      (model, [])
      [ updateMain
      , Game.update
      , Population.update
      ]
  in
    (newModel, Cmd.batch msgs)


updateMain : UpdateFunction
updateMain msg model =
  case msg of
    OnTime time ->
      ({ model | seed = initialSeed <| round time }, Cmd.none)
    SelectTab tab ->
      ({ model | selectedTab = tab }, Cmd.none)
    _ ->
      (model, Cmd.none)
