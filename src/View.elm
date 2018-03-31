module View exposing (..)

import Html exposing (Html, Attribute, div, text, input, h3)
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
renderState ({ state } as model) =
  let
    stateMsg =
      case state of
        Running ->
          "Running"
        Stopped ->
          "Stopped"

    stateText =
      h3 [] [ text stateMsg ]
  in
    stateText
