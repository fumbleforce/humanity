module Population.Update exposing (..)

import Model exposing (Model)
import Types exposing (..)
import Population.Common exposing (..)

handleSeedPerson : Model -> Model
handleSeedPerson model =
  let
    (newPerson, newSeed) = seedPerson model.seed model.date
  in
    { model
      | people = newPerson :: model.people
      , seed = newSeed
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SelectPerson pId ->
      ({ model | selectedPerson = Just pId }, Cmd.none)

    SeedPerson ->
      (handleSeedPerson model, Cmd.none)

    Tick dt ->
      case model.state of
        Running ->
          ({ model | people = stepPeople model }, Cmd.none )
        Stopped ->
          (model, Cmd.none)

    _ -> (model, Cmd.none)

