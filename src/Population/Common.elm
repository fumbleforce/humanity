module Population.Common exposing (..)

import Random exposing (int, initialSeed, step, minInt, maxInt, Seed)
import Maybe exposing (..)
import Dict
import List.Extra exposing (find)
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

isMale : Person -> Bool
isMale p =
  p.sex == Male

isFemale : Person -> Bool
isFemale p =
  p.sex == Female


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
  , bornAt = date
  , sex = sex
  , father = father
  , mother = mother
  , spouse = Nothing
  , fullness = 100
  , pregnantAt = Nothing
  , lifeLog = []
  }

randomId: Seed -> (Id, Seed)
randomId seed =
  step (int minInt maxInt) seed


seedPerson: Seed -> Date -> (Person, Seed)
seedPerson seed date =
  let
    (id, newSeed) = randomId seed
    mother = 0
    father = 0
    sex = determineSex id
    person = createPerson id father mother sex date
  in
    (person, newSeed)

determineSex : Id -> Sex
determineSex id =
  let
    (b, _) = step Random.bool (initialSeed id)
  in
    if b then Male else Female

determineId: Id -> Id
determineId id =
  let
    seed = initialSeed id
    (id_, _) = step (int minInt maxInt) seed
  in
    id_


eat : Person -> Person
eat p =
  { p
    | fullness  = p.fullness - settings.eatPerDay
  }

makeLifeLog : String -> Date -> LifeLogEntry
makeLifeLog event date =
  { event = event
  , date = date
  }

killOldPeople: Date -> Person -> Bool
killOldPeople date p =
  let
    age = getAge p date
  in
    age < settings.maxAge


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
    setPregnant p =
      { p | pregnantAt = Just date }

    setGaveBirth p =
      { p | pregnantAt = Nothing
          , lifeLog =
            makeLifeLog "birth" date :: p.lifeLog }

    isWife p =
      p.sex == Female && p.spouse /= Nothing --Some spouses are nothings, but they can still have babies.

    isPregnant p =
      p.pregnantAt /= Nothing

    canBeChildBearing p =
      ((isAdult date p) && (isWife p)) || (isPregnant p)

    canNotHaveBaby p =
      not (canBeChildBearing p)

    matchBirth log =
      log.event === Birth

    wantsChild p =
      let
        lastBirthLog = find matchBirth p.log
        marriageLog = find matchBirth p.log

        enoughTimeSinceLastBirth =
          case lastBirthLog of
            log -> (dateToDay log.date) > settings.daysOfCelibacyAfterPregnancy
            Nothing -> True

        enoughTimeSinceMarriage =
          case marriageLog of
            log -> (dateToDay log.date) > settings.daysOfCelibacyAfterMarriage
            Nothing -> False
      in
        enoughTimeSinceMarriage && enoughTimeSinceLastBirth

    isDue p =
      case p.pregnantAt of
        Just pregDate ->
          ((dateToDay date) - (dateToDay pregDate)) > settings.daysOfPregnancy
        Nothing ->
          False

    isNotDue p = not <| isDue p

    makeBaby fatherId motherId num =
      let
        babyId =
          determineId <| (randomGeneticIdent fatherId motherId) + num

        baby =
          createPerson babyId fatherId motherId (determineSex babyId) date
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

    (adultMarriedWomen_, otherPeople) =
      people
      |> List.partition canBeChildBearing

    (womenPregnant_, womentNotPregnant_) =
      adultMarriedWomen_
      |> List.partition isPregnant

    (womenDue_, womenNotDue) =
      womenPregnant_
      |> List.partition isDue

    babies =
      womenDue_
      |> List.map makeBabies
      |> List.concat

    (womenReady_, womenNotReady) =
      womentNotPregnant_
      |> List.partition wantsChild

    womenNewlyPregnant =
      womenReady_
      |> List.map setPregnant

    womenNoLongerPregnant =
      womenDue_
      |> List.map setGaveBirth
  in
    List.concat
      [ otherPeople
      , womenNotDue
      , womenNewlyPregnant
      , womenNoLongerPregnant
      , babies
      ]


matchmake: Date -> List Person -> List Person
matchmake date people =
  let
    isSingleAdult p =
      isAdult date p && p.spouse == Nothing

    singleAdults =
      List.filter isSingleAdult people

    (bachelors, bachelorsettes) =
      List.partition isMale singleAdults

    marry man woman = (man.id, woman.id)

    couples =
      List.map2 marry bachelors bachelorsettes

    wifeOf =
      Dict.fromList couples

    husbandOf =
      couples
      |> List.map (\(m, w) -> (w, m))
      |> Dict.fromList

    setSpouse p =
      if Dict.member p.id wifeOf then
        { p | spouse = Dict.get p.id wifeOf }
      else if Dict.member p.id husbandOf then
        { p | spouse = Dict.get p.id husbandOf }
      else
        p
  in
    List.map setSpouse people
