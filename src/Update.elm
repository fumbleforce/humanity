module Update exposing (..)

import Time

import Game exposing (Model, State(..), Msg(..))
import People.PeopleUpdate


startSim : Model -> Model
startSim model =
  { model
    | state = Running
  }

stopSim: Model -> Model
stopSim model =
  { model
    | state = Stopped
  }

stepSimulate : Time.Time -> Model -> Model
stepSimulate delta model =
  { model
    | people = People.PeopleUpdate.stepPeople model.people
    , day = model.day + 1
    , year = model.year + 1 / 365
  }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      ( model, Cmd.none )
    
    SpaceDown ->
      case model.state of
        Stopped ->
          ( startSim model, Cmd.none )
        Running ->
          ( stopSim model, Cmd.none )

    WindowSize newSize ->
      ( { model | windowDimensions = newSize }, Cmd.none )
    
    SelectPerson pId ->
      ({ model | selectedPerson = Just pId }, Cmd.none)
    
    Tick dt ->
      case model.state of
        Running ->
          ( stepSimulate dt model, Cmd.none )
        Stopped ->
          ( model, Cmd.none )

