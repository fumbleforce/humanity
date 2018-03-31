module Population.Update exposing (..)

import Model exposing (Model)
import Types exposing (..)
import Population.Common exposing (..)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SelectPerson pId ->
      ({ model | selectedPerson = Just pId }, Cmd.none)

    Tick dt ->
      case model.state of
        Running ->
          ({ model | people = stepPeople model }, Cmd.none )
        Stopped ->
          (model, Cmd.none)

    _ -> (model, Cmd.none)

