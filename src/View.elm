module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Settings exposing (settings)
import Model exposing (Model)
import Types exposing
  ( Msg(SelectTab)
  , GameState(Running, Stopped)
  , UIMainTab(TabPopulation, TabStatistics, TabGenepool, TabSettings))
import Population.View as Population
import Html.Events exposing (onMouseUp)

--manualMsg = "SPACE to start"
--textHeight = 24
--gameWidth = 600
--gameHeight = 400


view : Model -> Html Msg
view model =
  main_ []
    [ renderBackground
    , renderState model
    , renderTabMenu model
    , renderTabContent model
    ]

renderTabMenu : Model -> Html Msg
renderTabMenu model =
  let
    renderTabItem tab name =
      a
        [ class (if tab == model.selectedTab then "active" else "")
        , onMouseUp <| SelectTab tab ]
        [ text name]
  in
    nav []
      [ renderTabItem TabPopulation "Population"
      , renderTabItem TabStatistics "Statistics"
      , renderTabItem TabGenepool "Genepool"
      , renderTabItem TabSettings "Settings"
      ]

renderTabContent: Model -> Html Msg
renderTabContent ({ selectedTab } as model) =
  case selectedTab of
    TabPopulation -> Population.renderPopulation model
    TabStatistics -> Population.renderPopulationStatistics model
    TabGenepool -> Population.renderGenepool model
    TabSettings -> renderSettings



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
    section []
      [ stateText
      , dateText
      ]

renderSettings: Html Msg
renderSettings =
  let
    renderSetting key val =
      tr []
        [ td [] [ text key ]
        , td [] [ text <| toString <| val ]
        ]
  in
    table [] [ tbody []
      [ renderSetting "eatPerDay" settings.eatPerDay
      , renderSetting "maxAge" settings.maxAge
      , renderSetting "daysInYear" settings.daysInYear
      , renderSetting "adultAge" settings.adultAge
      , renderSetting "daysOfPregnancy" settings.daysOfPregnancy
      , renderSetting "daysOfCelibacyAfterPregnancy" settings.daysOfCelibacyAfterPregnancy
      , renderSetting "daysOfCelibacyAfterMarriage" settings.daysOfCelibacyAfterMarriage
      ]
    ]

