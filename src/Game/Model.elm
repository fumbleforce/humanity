module Game.Model exposing (..)

import Window
import Game.Types exposing (State(..))

type alias Model =
  { state : State
  , windowDimensions : Window.Size
  , day: Int
  , year: Float
  }


initialModel : Model
initialModel =
  { state = Stopped
  , windowDimensions = { width = 640, height = 480 }
  , day = 0
  , year = 0
  }
