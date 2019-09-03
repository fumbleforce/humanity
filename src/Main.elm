port module Main exposing (main)

import Html
import Json.Encode

import Model exposing (init, Model, encodeModel)
import Types exposing (Msg)
import View exposing (view)
import Update exposing (update)
import Subs exposing (subscriptions)

main : Program (Maybe Json.Encode.Value) Model Msg
main =
  Html.programWithFlags
    { init = init
    , view = view
    , update = updateWithStorage
    , subscriptions = subscriptions
    }


port setStorage : Json.Encode.Value -> Cmd msg


{-| We want to `setStorage` on every update. This function adds the setStorage
command for every step of the update function.
-}
updateWithStorage : Msg -> Model -> (Model, Cmd Msg)
updateWithStorage msg model =
  let
    (newModel, cmds) =
      update msg model
  in
    ( newModel
    , Cmd.batch [ setStorage (encodeModel newModel), cmds ]
    )