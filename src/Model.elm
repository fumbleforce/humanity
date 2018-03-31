module Model exposing (..)

import Task
import Window
import Json.Encode.Extra as EncodeExtra
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder, andThen)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)

import Types exposing (..)

type alias Model =
  { --Temporary
    state : GameState
  , windowDimensions : Window.Size
  , selectedPerson: Maybe Id

    -- Data
  , date: Date
  , people: List Person
  }


init : Maybe Encode.Value -> ( Model, Cmd Msg )
init savedModel =
  case savedModel of
    Just value ->
      Maybe.withDefault initialModel (Decode.decodeValue decodeModel value |> resultToMaybe) ! []
    _ ->
      initialModel ! []

resultToMaybe : Result String Model -> Maybe Model
resultToMaybe result =
  case result of
    Result.Ok model -> Just model
    Result.Err error -> Debug.log error Nothing


initialModel : Model
initialModel =
  { state = Stopped
  , windowDimensions = { width = 640, height = 480 }
  , selectedPerson = Nothing
  , date = { day = 0, year = 0 }
  , people = []
  }


getWindowSizeCommand : Cmd Msg
getWindowSizeCommand =
  Task.perform WindowSize Window.size


-- Decoders --

decodeModel : Decoder Model
decodeModel =
  decode Model
  |> hardcoded Stopped
  |> hardcoded { width = 640, height = 480 }
  |> hardcoded Nothing
  |> required "date" decodeDate
  |> required "people" (Decode.list decodePerson)

decodePerson : Decoder Person
decodePerson =
  decode Person
  |> required "id" Decode.int
  |> required "bornAt" decodeDate
  |> required "sex" decodeSex
  |> required "father" Decode.int
  |> required "mother" Decode.int
  |> required "spouse" (Decode.nullable Decode.int)
  |> required "fullness" Decode.float
  |> required "pregnantAt" (Decode.nullable decodeDate)

decodeDate : Decoder Date
decodeDate =
  decode Date
  |> required "day" Decode.int
  |> required "year" Decode.int

decodeSex : Decoder Sex
decodeSex =
  let
    innerDecode tag =
      case tag of
        "Male" -> Decode.succeed Male
        "Female" -> Decode.succeed Female
        _ -> Decode.fail (tag ++ " is not a recognized tag for Sex")
  in
    andThen innerDecode Decode.string

-- Encoders --

encodeModel : Model -> Encode.Value
encodeModel model =
  Encode.object
    [
      ("date", encodeDate model.date),
      ("people", Encode.list (List.map encodePerson model.people))
    ]

encodePerson : Person -> Encode.Value
encodePerson person =
  Encode.object
    [
      ("id", Encode.int person.id),
      ("bornAt", encodeDate person.bornAt),
      ("sex", encodeSex person.sex),
      ("father", Encode.int person.father),
      ("mother", Encode.int person.mother),
      ("spouse", (EncodeExtra.maybe Encode.int) person.spouse),
      ("fullness", Encode.float person.fullness),
      ("pregnantAt", (EncodeExtra.maybe encodeDate) person.pregnantAt)
    ]

encodeDate : Date -> Encode.Value
encodeDate date =
  Encode.object
    [
      ("year", Encode.int date.year),
      ("day", Encode.int date.day)
    ]

encodeSex : Sex -> Encode.Value
encodeSex sex =
  case sex of
    Male -> Encode.string "Male"
    Female -> Encode.string "Female"
