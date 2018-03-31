module Game.Update exposing (..)

import Msg as Main exposing (..)
import Game.Msg exposing (Msg(..))
import Game.Model exposing (Model)


update : Main.Msg -> Model -> Model
update msgFor game =
  case msgFor of
    MsgForGame msg ->
      updateGame msg game

    _ ->
      game


type alias FocusPort a =
    String -> Cmd a

updateCmd : FocusPort a -> Main.Msg -> Cmd a
updateCmd focus msg =
    case msg of
        _ ->
            Cmd.none


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
  { model
    | people = Population.Update.stepPeople model.people
    , day = model.day + 1
    , year = model.year + 1 / 365
  }

update : Msg -> Model -> Model
update msg model =
  { model
  | game = }


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

