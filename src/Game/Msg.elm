module Game.Msg exposing (Msg)

import Window
import Time

type Msg
  = Tick Time.Time
  | WindowSize Window.Size

