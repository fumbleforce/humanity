module Population.Model exposing (..)

import Population.Types exposing (Person, Id)

type alias Model =
  { people: List Person
  , selectedPerson: Maybe Id
  }


initialModel : Model
initialModel =
  { people = []
  , selectedPerson = Nothing
  }
