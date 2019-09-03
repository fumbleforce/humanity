module Rendering exposing ()

import Collage
  exposing
    ( filled
    , move
    , Shape
    , Form
    , rect
    , group
    , traced
    , solid
    , circle
    , toForm
    , collage
    , scale
    , path
    )
import Color exposing (Color, rgb, rgba, hsl, lightGray, darkGray)
import Element exposing (Element, layers, leftAligned)
import Types exposing (Positioned)
import Text
import Window

make : Color -> Positioned a -> Shape -> Form
make color entity shape =
  shape
    |> filled color
    |> move ( entity.x, entity.y )


noForm : Form
noForm =
  rect 0 0 |> filled (rgba 0 0 0 0)


txt : (Text.Text -> Text.Text) -> String -> Element
txt f =
  Text.fromString
    >> Text.color (rgb 0 0 0)
    >> Text.monospace
    >> f
    >> leftAligned

createRenderer : Window.Size -> Float -> Float -> Form -> Element
createRenderer { width, height } gameWidth gameHeight content =
  let
    -- to prevent scrolling down when user hits space bar
    height_ =
      height - 20

    gameScale =
      Basics.min (toFloat width / gameWidth)
        (toFloat height_ / gameHeight)
  in
    collage width height_ [ content |> scale gameScale ]
