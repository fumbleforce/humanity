module Population.Common exposing (..)

import Random exposing (int, initialSeed, step, minInt, maxInt, Seed)
import Maybe exposing (..)
import Dict
import Basics exposing (toFloat)

import Model exposing (Model)
import Types exposing (..)
import Settings exposing (settings)

dateToDay : Date -> Int
dateToDay date =
  date.year * settings.daysInYear + date.day

getAge : Person -> Date -> Float
getAge p date =
  let
    bornDay = dateToDay p.bornAt
    currentDay = dateToDay date
  in
    toFloat(currentDay - bornDay) / settings.daysInYear

isChild : Date -> Person -> Bool
isChild date p =
  let
    age = getAge p date
  in
    age < settings.adultAge

isAdult : Date -> Person -> Bool
isAdult date p =
  let
    age = getAge p date
  in
    age >= settings.adultAge


stepPerson: Person -> Person
stepPerson p =
  p
  |> eat

stepPeople : Model -> List Person
stepPeople { people, date } =
  let
    stepSinglePerson p =
      stepPerson p
  in
    people
    |> List.map stepSinglePerson
    |> List.filter (killOldPeople date)
    |> matchmake date
    |> makeBabies date


createPerson: Id -> Id -> Id -> Sex -> Date -> Person
createPerson id father mother sex date =
  { id = id
  , father = father
  , mother = mother
  , sex = sex
  , bornAt = date
  , fullness = 100
  , pregnantAt = Nothing
  , spouse = Nothing
  }


eat : Person -> Person
eat p =
  { p
    | fullness  = p.fullness - settings.eatPerDay
  }


killOldPeople: Date -> Person -> Bool
killOldPeople date p =
  let
    age = getAge p date
  in
    age < settings.maxAge


randomId: Id -> Id
randomId id =
  let
    seed = initialSeed id
    (id_, _) = step (int minInt maxInt) seed
  in
    id

randomSex : Id -> Sex
randomSex id =
  let
    (b, _) = step Random.bool (initialSeed id)
  in
    if b then Male else Female

randomBabyNumber: Id -> Int
randomBabyNumber id =
  let
    (num, _) = step (int 0 5) (initialSeed id)
  in
    num

randomGeneticIdent: Id -> Id -> Int
randomGeneticIdent fId mId =
  fId + mId

makeBabies: Date -> List Person -> List Person
makeBabies date people =
  let
    peopleById =
      people
      |> List.map (\p -> (p.id, p))
      |> Dict.fromList

    setPregnant p =
      { p | pregnantAt = Just date }

    isWife p =
      p.sex == Female && p.spouse /= Nothing

    canHaveBaby p =
      (isAdult date p) && p.pregnantAt == Nothing

    canNotHaveBaby p =
      (isChild date p) || p.pregnantAt /= Nothing

    fertileMarriedWomen =
      people
      |> List.filter isWife
      |> List.filter canHaveBaby

    pregnantAtWomen =
      fertileMarriedWomen
      |> List.map setPregnant

    otherPeople =
      people
      |> List.filter (\p -> not <| List.member p fertileMarriedWomen)

    makeBaby fatherId motherId num =
      let
        babyId =
          randomId <| (randomGeneticIdent fatherId motherId) + num

        baby =
          createPerson babyId fatherId motherId (randomSex babyId) date
      in
        baby

    makeBabies mother =
      let
        fatherId =
          case mother.spouse of
            Just p -> p
            Nothing -> 0

        motherId = mother.id

        geneticIdent = randomGeneticIdent fatherId motherId

        numBabies = randomBabyNumber geneticIdent
      in
        List.range 0 numBabies
        |> List.map (makeBaby fatherId motherId)

    babies =
      pregnantAtWomen
      |> List.map makeBabies
      |> List.concat

  in
    List.concat [otherPeople, pregnantAtWomen, babies]


matchmake: Date -> List Person -> List Person
matchmake date people =
  let
    isSingle p = p.spouse == Nothing

    singles =
      List.filter isSingle people

    bachelors =
      singles
      |> List.filter (\p -> p.sex == Male)
      |> List.map (\p -> p.id)

    bachelorsettes =
      singles
      |> List.filter (\p -> p.sex == Female)
      |> List.map (\p -> p.id)

    marry manId womanId = (manId, womanId)

    couples =
      List.map2 marry bachelors bachelorsettes

    wifeOf =
      couples
      |> Dict.fromList

    husbandOf =
      couples
      |> List.map (\(m, w) -> (w, m))
      |> Dict.fromList

    setWife p =
      if
        Dict.member p.id wifeOf
      then
        { p | spouse = Dict.get p.id wifeOf }
      else
        p

    setHusband p =
      if
        Dict.member p.id husbandOf
      then
        { p | spouse = Dict.get p.id husbandOf }
      else
        p
  in
    people
    |> List.map setHusband
    |> List.map setWife
