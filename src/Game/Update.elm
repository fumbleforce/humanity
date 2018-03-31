module Game.Update exposing (..)

import Time

import Settings exposing (settings)
import Types exposing (..)
import Model exposing (Model)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SpaceDown ->
      case model.state of
        Stopped ->
          (startGame model, Cmd.none)
        Running ->
          (stopGame model, Cmd.none)

    WindowSize newSize ->
      ({ model | windowDimensions = newSize }, Cmd.none)

    SelectPerson pId ->
      ({ model | selectedPerson = Just pId }, Cmd.none)

    Tick dt ->
      case model.state of
        Running ->
          (stepGame dt model, Cmd.none)
        Stopped ->
          (model, Cmd.none)

    _ -> (model, Cmd.none)


startGame : Model -> Model
startGame model =
  { model
    | state = Running
  }

stopGame: Model -> Model
stopGame model =
  { model
    | state = Stopped
  }

stepGame : Time.Time -> Model -> Model
stepGame delta model =
  let
    date = model.date
    daysInYear = settings.daysInYear
    newDate =
      if date.day == daysInYear then
        { day = 0, year = date.year + 1 }
      else
        { day = date.day + 1, year = date.year }
  in
    { model | date = newDate }