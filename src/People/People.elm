module People.People exposing (..)

import Random exposing (int, initialSeed, step, minInt, maxInt, Seed)
import Maybe exposing (..)
import Dict

import People.Person exposing (Person, Id, Sex(..))


peopleConfig =
  { personW = 1
  , personH = 1
  , eatPerDay = 1
  , maxAge = 1
  }

adultAge = 0.1

initialPeople =
  [ createPerson 1234 0 0 Male
  , createPerson 22 0 0 Male
  , createPerson 333 0 0 Male
  , createPerson 4444 0 0 Male
  , createPerson 55554 0 0 Male
  , createPerson 666666 0 0 Female
  , createPerson 777700 0 0 Female
  , createPerson 880000 0 0 Female
  , createPerson 9999 0 0 Female
  , createPerson 100 0 0 Female
  ]


createPerson: Id -> Id -> Id -> Sex -> Person
createPerson id father mother sex =
  { id = id
  , father = father
  , mother = mother
  , sex = sex
  , age = 0
  , fullness = 100
  , pregnant = False
  , spouse = Nothing
  }

isChild p =
  p.age < adultAge

isAdult p =
  p.age >= adultAge

makeOlder : Person -> Person
makeOlder p =
  { p
    | age  = p.age + 1 / 100
  }

eat : Person -> Person
eat p =
  { p
    | fullness  = p.fullness - peopleConfig.eatPerDay
  }


killOldPeople: Person -> Bool
killOldPeople p =
  p.age < peopleConfig.maxAge


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

makeBabies: List Person -> List Person
makeBabies people =
  let
    peopleById =
      people
      |> List.map (\p -> (p.id, p))
      |> Dict.fromList
  
    setPregnant p =
      { p | pregnant = True }
    
    isWife p =
      p.sex == Female && p.spouse /= Nothing
    
    canHaveBaby p =
      (isAdult p) && not p.pregnant
    
    canNotHaveBaby p =
      (isChild p) || p.pregnant
    
    fertileMarriedWomen =
      people
      |> List.filter isWife
      |> List.filter canHaveBaby
    
    pregnantWomen =
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
          createPerson babyId fatherId motherId (randomSex babyId)
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
      pregnantWomen
      |> List.map makeBabies
      |> List.concat
    
  in
    List.concat [otherPeople, pregnantWomen, babies]


matchmake: List Person -> List Person
matchmake people =
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
    
