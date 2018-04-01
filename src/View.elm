module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

import Model exposing (Model)
import Types exposing (..)
import Population.View exposing (renderPopulation)

--manualMsg = "SPACE to start"
--textHeight = 24
--gameWidth = 600
--gameHeight = 400


view : Model -> Html Msg
view model =
  div []
    [ renderBackground
    , renderState model
    , renderPopulation model
    ]

renderBackground : Html Msg
renderBackground =
  div [ class "background" ] []


renderState : Model -> Html Msg
renderState ({ state, date, seed } as model) =
  let
    stateMsg =
      case state of
        Running ->
          "Running"
        Stopped ->
          "Stopped"

    stateText =
      h3 [] [ text stateMsg ]

    dateText =
      h4 []
        [ text " Day "
        , text <| toString date.day
        , text " Year "
        , text <| toString date.year]
  in
    div []
      [ stateText
      , dateText
      ]
