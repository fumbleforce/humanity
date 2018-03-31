module Population.View exposing (..)

import List exposing (length, map, filter, sum)
import Color exposing (Color, rgb)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onMouseEnter)

import Model exposing (Model)
import Types exposing (..)
import Population.Common exposing (isChild, isAdult, getAge)

personColor = rgb 0 0 0


renderPopulation : Model -> Html Msg
renderPopulation ({ people, selectedPerson, date } as model) =
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

    renderPopulationStatistics =
      table [ class "border" ]
        [ tbody []
          [ tr []
            [ td [] [ text "Pregnant" ]
            , td [] [ text <| toString <| peopleStatistics.numPregnant ]
            ]
          , tr []
            [ td [] [ text "Adults" ]
            , td [] [ text <| toString <| peopleStatistics.numAdults ]
            ]
          , tr []
            [ td [] [ text "Total" ]
            , td [] [ text <| toString <| peopleStatistics.num ]
            ]
          , tr []
            [ td [] [ text "Children" ]
            , td [] [ text <| toString <| peopleStatistics.numChild ]
            ]
          , tr []
            [ td [] [ text "Average age" ]
            , td [] [ text <| toString <| peopleStatistics.avgAge ]
            ]
          ]
        ]

    renderPerson p =
      let
        color =
          case selectedPerson of
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
          "p1 bg-white border inline-block " ++ color
      in
        div [ class classes, onMouseEnter (SelectPerson p.id) ]
          [ table []
            [ tbody []
              [ tr []
                [ td [] [ text "ID" ]
                , td [] [ text <| toString <| p.id ]
                ]
              , tr []
                [ td [] [ text "Mother" ]
                , td [] [ text <| toString <| p.mother ]
                ]
              , tr []
                [ td [] [ text "Father" ]
                , td [] [ text <| toString <| p.father ]
                ]
              , tr []
                [ td [] [ text "Spouse" ]
                , td [] [ text <| toString <| p.spouse ]
                ]
              , tr []
                [ td [] [ text "Sex" ]
                , td [] [ text <| toString <| p.sex ]
                ]
              , tr []
                [ td [] [ text "Age" ]
                , td [] [ text <| toString <| round <| getAge p date ]
                ]
              , tr []
                [ td [] [ text "Preg" ]
                , td [] [ text <| toString <| p.pregnantAt ]
                ]
              ]
            ]
          ]

    renderPopulationElements =
      people
      |> List.map renderPerson
  in
    div []
      [ renderPopulationStatistics
      , div [] renderPopulationElements
      ]
