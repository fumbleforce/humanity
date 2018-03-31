module Msg exposing (..)

import Game.Msg as Game
import Population.Msg as Population

type Msg
  = NoOp
  | GameMsg Game.Msg
  | PopulationMsg Population.Msg
