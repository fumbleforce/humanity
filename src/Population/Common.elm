module Population.Common exposing (
  getAge
  , isChild
  , isAdult
  , isMale
  , isFemale
  , isPregnant
  , isWife
  , eat
  , makeLifeLog
  , matchmake
  , killOldPeople)

import Dict
import Basics exposing (toFloat)
import Types exposing (Individual, Date, Sex(Male, Female), LifeLogEntry, LifeLogEvent(..))
import Settings exposing (settings)
import Game.Common exposing (dateToDay, dayToDate)

getAge : Individual -> Date -> Float
getAge p date =
  let
    bornDay = dateToDay p.bornAt
    currentDay = dateToDay date
  in
    toFloat(currentDay - bornDay) / settings.daysInYear

isChild : Date -> Individual -> Bool
isChild date p =
  let
    age = getAge p date
  in
    age < settings.adultAge

isAdult : Date -> Individual -> Bool
isAdult date p =
  let
    age = getAge p date
  in
    age >= settings.adultAge

isMale : Individual -> Bool
isMale p =
  p.sex == Male

isFemale : Individual -> Bool
isFemale p =
  p.sex == Female

isPregnant : Individual -> Bool
isPregnant p =
  p.pregnantAt /= Nothing

isWife : Individual -> Bool
isWife p =
  p.sex == Female && p.spouse /= Nothing --Some spouses are nothings, but they can still have babies.


eat : Individual -> Individual
eat p =
  { p
    | fullness  = p.fullness - settings.eatPerDay
  }

makeLifeLog : LifeLogEvent -> Date -> LifeLogEntry
makeLifeLog event date =
  { event = event
  , date = date
  }

killOldPeople: Date -> Individual -> Bool
killOldPeople date p =
  let
    age = getAge p date
  in
    age < settings.maxAge

matchmake: Date -> List Individual -> List Individual
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
        { p | spouse = Dict.get p.id wifeOf
            , lifeLog = makeLifeLog Marriage date :: p.lifeLog }
      else if Dict.member p.id husbandOf then
        { p | spouse = Dict.get p.id husbandOf
            , lifeLog = makeLifeLog Marriage date :: p.lifeLog }
      else
        p
  in
    List.map setSpouse people
