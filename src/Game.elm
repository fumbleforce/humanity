module Game exposing (..)

import Window
import Time
import Maybe

import People.Person exposing (Person, Id)

type State
  = Stopped
  | Running


type Msg
  = NoOp
  | SpaceDown
  | WindowSize Window.Size
  | Tick Time.Time
  | SelectPerson Id


type alias Model =
  { state : State
  , windowDimensions : Window.Size
  , people: List Person
  , selectedPerson: Maybe Id
  , day: Int
  , year: Float
  }