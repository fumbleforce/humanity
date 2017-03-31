module People.PeopleView exposing (..)

import List exposing (length, map, filter, sum)
import Color exposing (Color, rgb)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onMouseEnter)

import Game exposing (Model, Msg(..))
import People.Person exposing (Person, Sex(..))
import People.People exposing (peopleConfig, isChild, isAdult)

personColor = rgb 0 0 0


renderPeople : Model -> Html Msg
renderPeople ({ people, selectedPerson } as model) =
  let
    peopleStatistics =
      { numPregnant =
          people
          |> filter (\p -> p.pregnant)
          |> length
      , numChild =
          people
          |> filter isChild
          |> length
      , num =
          people
          |> length
      , numAdults =
          people
          |> filter isAdult
          |> length
      , avgAge =
          people
          |> List.map (\p -> p.age)
          |> sum
          |> \ages -> ages / (toFloat (length people))
      }
    
    renderPeopleStatistics =
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
                , td [] [ text <| toString <| round <| p.age * 100 ]
                ]
              , tr []
                [ td [] [ text "Preg" ]
                , td [] [ text <| toString <| p.pregnant ]
                ]
              ]
            ]
          ]
    
    renderPeopleElements =
      people
      |> List.map renderPerson
  in
    div []
      [ renderPeopleStatistics
      , div [] renderPeopleElements
      ]
