module Population.Reproduction exposing (controlPregnancy, makeBabies)

import Tuple
import Settings exposing (settings)
import Utils.List exposing (mapWithMemo, mapExpandWithMemo)
import List.Extra exposing (find)
import Types exposing
  ( Id
  , Date
  , Individual
  , LifeLogEvent(GaveBirth, Marriage))
import Game.Common exposing (dateToDay, dayToDate)
import Population.Genetics exposing (makeBaby, god)
import Population.Common exposing (isWife, isAdult, isPregnant, makeLifeLog)


enoughTimeSinceLastBirth: Date -> Individual -> Bool
enoughTimeSinceLastBirth date p =
  let
    matchBirth log =
      log.event == GaveBirth
    lastBirthEntry = find matchBirth p.lifeLog
  in
    case lastBirthEntry of
      Just entry ->
        let
          birthDay = dateToDay entry.date
          currentDay = dateToDay date
          diff = currentDay - birthDay
        in
          diff > settings.daysOfCelibacyAfterPregnancy
      Nothing -> True

enoughTimeSinceMarriage: Date -> Individual -> Bool
enoughTimeSinceMarriage date p =
  let
    matchMarriage log =
      log.event == Marriage
    marriageEntry = find matchMarriage p.lifeLog
  in
    case marriageEntry of
      Just entry ->
        let
          marriageDay = dateToDay entry.date
          currentDay = dateToDay date
          diff = currentDay - marriageDay
        in
          diff > settings.daysOfCelibacyAfterMarriage
      Nothing -> False


canHaveChild: Date -> Individual -> Bool
canHaveChild date p =
  (isAdult date p)
  && (isWife p)
  && enoughTimeSinceMarriage date p
  && enoughTimeSinceLastBirth date p


controlPregnancy : Date -> Individual -> Individual
controlPregnancy date p =
  let
    setPregnant p =
      { p | pregnantAt = Just date }

    setUnPregnant p =
      { p | pregnantAt = Nothing
          , lifeLog =
            makeLifeLog GaveBirth date :: p.lifeLog }

    isDue p =
      case p.pregnantAt of
        Just pregDate ->
          ((dateToDay date) - (dateToDay pregDate)) > settings.daysOfPregnancy
        Nothing ->
          False

    handlePregnant p =
      if isDue p then
        setUnPregnant p
      else
        p
  in
    if isPregnant p then handlePregnant p
    else if canHaveChild date p then setPregnant p
    else p

makeOneBaby: Date -> Individual -> Individual -> Id -> Individual
makeOneBaby date father mother babyId =
  let
    birthDate =
      dayToDate <| dateToDay date + settings.daysOfPregnancy

    baby =
      makeBaby babyId father mother birthDate
  in
    baby

makeBabiesOfMother: Date -> List Individual -> Id -> Individual -> (List Individual, Id)
makeBabiesOfMother date people prevId mother =
  let
    numBabies = 1

    father = case mother.spouse of
      Just fatherId ->
        case find (\ p -> p.id == fatherId) people of
          Just father -> father
          Nothing -> god -- hallelujah!
      Nothing -> god -- hallelujah!

    mothersBabies =
      List.range (prevId + 1) (prevId + numBabies)
      |> List.map (makeOneBaby date father mother)
  in
    (mothersBabies, prevId + numBabies)

makeBabies: Date -> Id -> List Individual -> (List Individual, Id)
makeBabies date lastIndividualId people =
  let
    gotPregnantToday p =
      case p.pregnantAt of
        Just pregnantAt ->
          dateToDay pregnantAt == dateToDay date
        Nothing ->
          False

    (womentPregnantToday, otherPeople) =
      people
      |> List.partition gotPregnantToday

    babies =
      womentPregnantToday
      |> mapExpandWithMemo (makeBabiesOfMother date people) lastIndividualId
      |> Tuple.first

    newPeopple =
      List.concat
        [ otherPeople
        , womentPregnantToday
        , babies
        ]

    newLastIndividualId =
      case babies of
        last :: _ -> last.id
        [] -> lastIndividualId

  in
    (newPeopple, newLastIndividualId)
