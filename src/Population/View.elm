module Population.View exposing
  ( renderPopulationStatistics
  , renderPopulation
  , renderGenepool)

import List exposing (length, map, filter, sum)
import List.Extra exposing (find, group)
import Color exposing (Color, rgb)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onMouseEnter, onMouseUp)

import Model exposing (Model)
import Types exposing (Msg(SelectIndividual, SeedIndividual))
import Population.Common exposing (isChild, isAdult, getAge)

personColor : Color
personColor = rgb 0 0 0

renderStringRow: String -> a -> Html Msg
renderStringRow key val =
  tr []
    [ td [] [ text key ]
    , td [] [ text <| toString <| val ]
    ]

renderAnyRow: String -> List (Html Msg) -> Html Msg
renderAnyRow key val =
  tr []
    [ td [] [ text key ]
    , td [] val
    ]

renderPopulationStatistics: Model -> Html Msg
renderPopulationStatistics ({ people, date } as model) =
  let
    peopleStatistics =
      { numPregnant =
          people
          |> filter (\p -> p.pregnantAt /= Nothing)
          |> length
      , numChild =
          people
          |> filter (isChild date)
          |> length
      , num =
          people
          |> length
      , numAdults =
          people
          |> filter (isAdult date)
          |> length
      , avgAge =
          people
          |> List.map (\p -> (getAge p date))
          |> sum
          |> \ages -> ages / (toFloat (length people))
      }
  in
    table []
      [ tbody []
        [ renderStringRow "Pregnant" peopleStatistics.numPregnant
        , renderStringRow "Adults" peopleStatistics.numAdults
        , renderStringRow "Total" peopleStatistics.num
        , renderStringRow "Children" peopleStatistics.numChild
        , renderStringRow "Average age" peopleStatistics.avgAge
        ]
      ]

renderPopulation : Model -> Html Msg
renderPopulation ({ people, selectedIndividual, date } as model) =
  let
    renderIndividual p =
      let
        color =
          case selectedIndividual of
            Nothing -> ""
            Just selectedId ->
              if selectedId == p.id then
                "white bg-blue"
              else if Just selectedId == p.spouse then
                "white bg-red"
              else if selectedId == p.father then
                "white bg-orange"
              else if selectedId == p.mother then
                "white bg-yellow"
              else
                ""

        classes =
          "p1 bg-white border " ++ color
      in
        tr [ class classes, onMouseEnter (SelectIndividual p.id) ]
          [ td [] [ text <| toString <| p.id ]
          , td [] [ text <| toString <| p.mother ]
          , td [] [ text <| toString <| p.father ]
          , td [] [ text <| toString <| p.spouse ]
          , td [] [ text <| toString <| p.sex ]
          , td [] [ text <| toString <| round <| getAge p date ]
          , td [] [ text <| toString <| p.pregnantAt ]
          ]

    renderPopulationElements =
      table []
        [ thead []
          [ tr []
            [ th [] [ text "ID" ]
            , th [] [ text "Mother" ]
            , th [] [ text "Father" ]
            , th [] [ text "Spouse" ]
            , th [] [ text "Sex" ]
            , th [] [ text "Age" ]
            , th [] [ text "Preg" ]
            ]
          ]
        , tbody []
            ( people
              |> List.sortBy .id
              |>List.map renderIndividual)
        ]


    renderLifeLogEntry entry =
      div [] [text <| toString entry]

    renderPersonDetails person =
      table []
        [ tbody []
          [ renderStringRow "ID" person.id
          , renderStringRow "Genome" person.genome
          , renderStringRow "Born" person.bornAt
          , renderStringRow "Age" (getAge person date)
          , renderStringRow "Sex" person.sex
          , renderStringRow "Father" person.father
          , renderStringRow "Mother" person.mother
          , renderStringRow "Spouse" person.spouse
          , renderStringRow "Pregnant" person.pregnantAt
          , renderAnyRow "Life" (List.map renderLifeLogEntry person.lifeLog)
          ]
        ]

    renderPersonIfSelected =
      case selectedIndividual of
        Nothing -> div [][]
        Just selectedId ->
          case find (\ p -> p.id == selectedId) people of
            Nothing -> div [][]
            Just person -> renderPersonDetails person

    renderSeedButtons =
      button [onMouseUp SeedIndividual] [ text "Individual" ]
  in
    section []
      [ div [class "grid two"]
        [ div [class "col"]
          [ renderSeedButtons
          , renderPopulationElements
          ]
        , div [class "col"]
          [ renderPersonIfSelected
          ]
        ]
      ]


renderGenepool: Model -> Html Msg
renderGenepool ({ people } as model) =
  let
    mapGene gene =
      people
      |> List.map .genome
      |> List.map (getAllele gene)
      |> List.concat
      |> group

    genes =
      geneSequence
      |> List.map mapGene
      |> zip geneSequence

    renderAllele name count =
      div []
        [ text <| (toString name ++ ":")
        , text <| toString <| count
        ]

    renderGeneName gene =
      h3 [] [ text <| toString gene ]

    renderGene (gene, groupedOccurences) =
      let
        renderAlleleList grouped =
          case grouped of
            [] -> div [] []
            allele :: [] -> renderAllele allele 1
            allele :: matches -> renderAllele allele (length matches + 1)
      in
        div
          []
          [ renderGeneName gene
          , div [] (List.map renderAlleleList groupedOccurences)
          ]
  in
    div []
      (List.map renderGene genes)
