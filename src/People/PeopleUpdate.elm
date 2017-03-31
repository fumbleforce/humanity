module People.PeopleUpdate exposing (..)

import People.Person exposing (Person, Sex(..))
import People.People exposing (peopleConfig
  , createPerson
  , makeOlder
  , eat
  , killOldPeople
  , makeBabies
  , matchmake
  )


stepPerson: Person -> Person
stepPerson p =
  p
  |> makeOlder
  |> eat

stepPeople : List Person -> List Person
stepPeople people =
  let
    stepSinglePerson p =
      stepPerson p

    people_ =
      people
      |> List.map stepSinglePerson
      |> List.filter killOldPeople
      |> matchmake
      |> makeBabies
  in
    people_
