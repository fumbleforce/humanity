module Tests.Population.Common exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Population.Common exposing (makeBabies)
import Types exposing (..)

suite : Test
suite =
  describe "Population Common"
    [ describe "makeBabies"
      [ test "does not duplicate people like crazy" <|
        \_ ->
          let
            date = { day = 1, year = 17 }
            people =
              [ { id = 1
                , father = 0
                , mother = 0
                , sex = Male
                , bornAt = { year = 0, day = 0}
                , fullness = 100
                , pregnantAt = Nothing
                , spouse = Just 2
                }
              , { id = 2
                , father = 0
                , mother = 0
                , sex = Female
                , bornAt = { year = 0, day = 0}
                , fullness = 100
                , pregnantAt = Nothing
                , spouse = Just 1
                }
              , { id = 3
                , father = 0
                , mother = 0
                , sex = Male
                , bornAt = { year = 0, day = 0}
                , fullness = 100
                , pregnantAt = Nothing
                , spouse = Just 4
                }
                , { id = 4
                , father = 0
                , mother = 0
                , sex = Female
                , bornAt = { year = 0, day = 0}
                , fullness = 100
                , pregnantAt = Just { day = 0, year = 17}
                , spouse = Just 4
                }
              ]
          in
            Expect.equal
              (List.length (makeBabies date people))
              (List.length people)

      ---- Expect.equal is designed to be used in pipeline style, like this.
      --, test "reverses a known string" <|
      --  \_ ->
      --    "ABCDEFG"
      --      |> String.reverse
      --      |> Expect.equal "GFEDCBA"

      ---- fuzz runs the test 100 times with randomly-generated inputs!
      --, fuzz string "restores the original string if you run it again" <|
      --  \randomlyGeneratedString ->
      --    randomlyGeneratedString
      --      |> String.reverse
      --      |> String.reverse
      --      |> Expect.equal randomlyGeneratedString
      ]
    ]