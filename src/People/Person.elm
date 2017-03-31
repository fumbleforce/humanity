module People.Person exposing (..)

import Maybe

type Sex
  = Male
  | Female

type alias Id = Int

type alias Person =
  { id: Id
  , father: Id
  , mother: Id
  , age: Float
  , sex: Sex
  , fullness: Float
  , pregnant: Bool
  , spouse: Maybe Id
  }
