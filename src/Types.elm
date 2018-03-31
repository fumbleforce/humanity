module Types exposing (..)

type alias Positioned entity =
  { entity | x : Float, y : Float }

type alias Aged entity =
  { entity | age : Float }

type alias Welfare entity =
  { entity | fullness : Float }