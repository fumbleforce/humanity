module Population.Genetics exposing (makeBaby, god, seedIndividual)

import Settings exposing (settings)
import Random exposing (initialSeed, int, bool, step, Seed)
import Types exposing (Individual, Id, Date, Gene(..), Genome, Allele, Sex(..))
import List.Extra exposing (find, getAt, findIndex)
import Utils.List exposing (zipWithSeed, mapWithSeed)
import Game.Common exposing (dayToDate, dateToDay)

geneSequence : List Gene
geneSequence =
  [ SexGene
  , BeautyGene
  , FertilityGene
  , MutationGene
  ]

getAllele : Gene -> Genome -> Allele
getAllele gene genome =
  let
    genePosition = findIndex (\t -> t == gene) geneSequence
  in
    case genePosition of
      Just position ->
        case getAt position genome of
          Just allele -> allele
          Nothing -> 0
      Nothing ->
        0

--getGeneExpression geneType genome =

determineChildCount: Genome -> Int
determineChildCount genome =
  let
    allele = getAllele FertilityGene genome
  in
    allele % 5

pickAGene: Allele -> Allele -> Seed -> (Allele, Seed)
pickAGene fGene mGene seed =
  let
    (pickFirst, nextSeed) = step bool seed

    pickedGene = if pickFirst
      then
        fGene
      else
        mGene
  in (pickedGene, nextSeed)


getMutationSeed: Id -> Genome -> Genome -> Seed
getMutationSeed id fGenome mGenome =
  let
    fAllele = getAllele MutationGene fGenome
    mAllele = getAllele MutationGene mGenome
  in
    initialSeed (mAllele + fAllele + id)

determineGeneticInheritance: Id -> Genome -> Genome -> Genome
determineGeneticInheritance id fGenome mGenome =
  let
    mutationSeed = getMutationSeed id fGenome mGenome
    (genome, _) = zipWithSeed pickAGene mutationSeed fGenome mGenome
  in
    genome

determineSex : Genome -> Sex
determineSex genome =
  let
    allele = getAllele SexGene genome
  in
    if allele % 2 == 1 then
      Male
    else
      Female

-- Needed to trick compiler into using correct types
createDefaultAllele: Gene -> Allele
createDefaultAllele _ = 0

randomAllele: Allele -> Seed -> (Allele, Seed)
randomAllele _ seed =
  step (Random.int 0 1000000) seed

randomGenome: Seed -> (Genome, Seed)
randomGenome seed =
  geneSequence
  |> List.map createDefaultAllele
  |> mapWithSeed randomAllele seed

createIndividualWithGenome: Id -> Id -> Id -> Genome -> Date -> Individual
createIndividualWithGenome id fatherId motherId genome date =
  { id = id
  , bornAt = date
  , genome = genome
  , sex = determineSex genome
  , father = fatherId
  , mother = motherId
  , spouse = Nothing
  , fullness = 100
  , pregnantAt = Nothing

  , lifeLog = []
  }

god: Individual
god =
  let
    (p, seed) = seedIndividual
      (initialSeed 0) -- God is unchanging and eternal, you know
      { day = 0, year = 0 }
      0
  in
    p

randomDayAge: Seed -> (Int, Seed)
randomDayAge seed =
  step (int 0 (settings.maxAge * settings.daysInYear)) seed


seedIndividual: Seed -> Date -> Id -> (Individual, Seed)
seedIndividual seed date id =
  let
    (dayAge, newSeed) = randomDayAge seed
    birthDate = dayToDate <| (dateToDay date) - dayAge
    motherId = 0
    fatherId = 0
    (genome, _) = randomGenome seed

    person = createIndividualWithGenome
      id
      fatherId
      motherId
      genome
      birthDate
  in
    (person, newSeed)


makeBaby: Id -> Individual -> Individual -> Date -> Individual
makeBaby id mother father date =
  let
    genome = determineGeneticInheritance id mother.genome father.genome
  in
    createIndividualWithGenome
      id
      father.id
      mother.id
      genome
      date
