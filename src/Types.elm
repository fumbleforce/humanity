module Types exposing (..)

import Time
import Dict exposing (Dict)
import Window
import Time


type Msg
  = NoOp
  | Tick Time.Time
  | SpaceDown
  | WindowSize Window.Size
  | SelectPerson Int
  | SeedPerson
  | OnTime Time.Time


type GameState
  = Stopped
  | Running


type Sex
  = Male
  | Female

type alias Id = Int

type alias Date =
  { day: Int
  , year: Int
  }

type alias LifeLogEntry =
  { event: String
  , date: Date
  }

type LifeLogEvent
  = Birth
  | Marriage
  | Pregnant

type alias Person =
  { id: Id

  -- Personalia
  , bornAt: Date
  , sex: Sex

  -- Relations
  , father: Id
  , mother: Id
  , spouse: Maybe Id

  -- State
  , fullness: Float
  , pregnantAt: Maybe Date
  , lifeLog: List LifeLogEntry
  }


type alias Positioned entity =
  { entity | x : Float, y : Float }

type alias Aged entity =
  { entity | age : Float }

type alias Welfare entity =
  { entity | fullness : Float }