module Population.Update exposing (update)

import Model exposing (Model)
import Types exposing (
  Msg(SelectIndividual, SeedIndividual, Tick),
  Individual,
  Date,
  Id,
  GameState(Running, Stopped))
import Population.Common exposing (matchmake, eat, killOldPeople)
import Population.Reproduction exposing (controlPregnancy, makeBabies)
import Population.Genetics exposing (seedIndividual)


handleSeedIndividual : Model -> Model
handleSeedIndividual model =
  let
    ({ seed, date, peopleLastId, people }) = model
    id = peopleLastId + 1
    (newIndividual, newSeed) = seedIndividual seed date id
  in
    { model
      | people = newIndividual :: people
      , peopleLastId = id
      , seed = newSeed
    }


stepIndividual: Date -> Individual -> Individual
stepIndividual date p =
  p
  |> eat
  |> controlPregnancy date

stepPeople : Model -> (List Individual, Id)
stepPeople { people, date, peopleLastId } =
  people
  |> List.map (stepIndividual date)
  |> List.filter (killOldPeople date)
  |> matchmake date
  |> makeBabies date peopleLastId



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SelectIndividual pId ->
      ({ model | selectedIndividual = Just pId }, Cmd.none)

    SeedIndividual ->
      (handleSeedIndividual model, Cmd.none)

    Tick dt ->
      case model.state of
        Running ->
          let
            (people, peopleLastId) = stepPeople model
          in
            ({ model
              | people = people
              , peopleLastId = peopleLastId }
            , Cmd.none )
        Stopped ->
          (model, Cmd.none)

    _ -> (model, Cmd.none)

