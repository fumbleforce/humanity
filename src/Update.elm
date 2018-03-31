module Update exposing (..)

import Time

import Msg exposing (..)
import Model exposing (Model)
import Game.Update as Game
import Population.Update as Population


type alias FocusPort =
    String -> Cmd Msg


update : Msg -> Model -> Model
update msg model =
    { model
        | game = Game.update msg model.game
        , population = Population.update msg model.population
    }


updateWithCmd : FocusPort -> Msg -> Model -> ( Model, Cmd Msg )
updateWithCmd focus msg model =
    ( update msg model, updateCmd focus msg )


updateCmd : FocusPort -> Msg -> Cmd Msg
updateCmd focus msg =
    Cmd.batch
        [ Game.updateCmd focus msg
        , Population.updateCmd focus msg
        ]
