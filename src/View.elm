module View exposing (..)

import Html exposing (Html, Attribute, div, text, input, h3)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck)
import Text

import Model exposing (Model)
import Population.View exposing (renderPeople)


viewConfig =
  { manualMsg = "SPACE to start"
  , textHeight = 24
  , gameWidth = 600
  , gameHeight = 400
  }


view : Model -> Html Msg
view model =
  div []
    [ renderBackground
    , renderState model
    , renderPeople model
    ]

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
