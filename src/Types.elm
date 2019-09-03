module Types exposing (
  Msg(..),
  GameState(..),
  Sex(..),
  Id,
  Allele,
  Genome,
  Gene(..),
  Date,
  LifeLogEvent(..),
  LifeLogEntry,
  Individual,
  Positioned,
  Aged,
  UIMainTab(..),
  Welfare)

import Time
import Window
import Time


type Msg
  = NoOp
  | Tick Time.Time
  | SpaceDown
  | WindowSize Window.Size
  | SelectIndividual Int
  | SelectTab UIMainTab
  | SeedIndividual
  | OnTime Time.Time


type UIMainTab
  = TabPopulation
  | TabStatistics
  | TabGenepool
  | TabSettings

type GameState
  = Stopped
  | Running


type Sex
  = Male
  | Female

type alias Id = Int

type alias Allele = Int
type alias Genome = List Allele

type Gene
  = SexGene
  | BeautyGene
  | MutationGene
  | FertilityGene

type alias Date =
  { day: Int
  , year: Int
  }

type alias LifeLogEntry =
  { event: LifeLogEvent
  , date: Date
  }

type LifeLogEvent
  = GaveBirth
  | Marriage
  | Pregnant

type alias Individual =
  { id: Id

  -- Individualalia
  , genome: Genome
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