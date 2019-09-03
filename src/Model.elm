module Model exposing (Model, init, encodeModel)

import Task
import Window
import Random exposing (Seed, initialSeed)
import Json.Encode.Extra as EncodeExtra
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder, andThen)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Time

import Types exposing (
  Msg(..),
  GameState(..),
  Date,
  Individual,
  Id,
  Sex(..),
  LifeLogEntry,
  UIMainTab(TabPopulation),
  LifeLogEvent(GaveBirth, Marriage, Pregnant))

type alias Model =
  { --Temporary
    state : GameState
  , windowDimensions : Window.Size
  , selectedIndividual: Maybe Id
  , selectedTab: UIMainTab
  , seed: Seed

    -- Data
  , date: Date
  , people: List Individual
  , peopleLastId: Id
  }


init : Maybe Encode.Value -> ( Model, Cmd Msg )
init savedModel =
  (decodeSavedModel savedModel, getTimeCommand)

decodeSavedModel : Maybe Encode.Value -> Model
decodeSavedModel savedModel =
  case savedModel of
    Just value ->
      Maybe.withDefault initialModel (Decode.decodeValue decodeModel value |> resultToMaybe)
    _ ->
      initialModel

resultToMaybe : Result String Model -> Maybe Model
resultToMaybe result =
  case result of
    Result.Ok model -> Just model
    Result.Err error -> Debug.log error Nothing


initialModel : Model
initialModel =
  { state = Stopped
  , windowDimensions = { width = 640, height = 480 }
  , selectedIndividual = Nothing
  , selectedTab = TabPopulation
  , seed = initialSeed 0
  , date = { day = 0, year = 0 }
  , people = []
  , peopleLastId = 0
  }


getTimeCommand : Cmd Msg
getTimeCommand =
  Task.perform OnTime Time.now


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
  |> hardcoded TabPopulation
  |> hardcoded (initialSeed 1)
  |> required "date" decodeDate
  |> required "people" (Decode.list decodeIndividual)
  |> required "peopleLastId" Decode.int

decodeIndividual : Decoder Individual
decodeIndividual =
  decode Individual
  |> required "id" Decode.int
  |> required "genome" (Decode.list Decode.int)
  |> required "bornAt" decodeDate
  |> required "sex" decodeSex
  |> required "father" Decode.int
  |> required "mother" Decode.int
  |> required "spouse" (Decode.nullable Decode.int)
  |> required "fullness" Decode.float
  |> required "pregnantAt" (Decode.nullable decodeDate)
  |> required "lifeLog" (Decode.list decodeLifeLogEntry)


decodeDate : Decoder Date
decodeDate =
  decode Date
  |> required "day" Decode.int
  |> required "year" Decode.int

decodeLifeLogEntry : Decoder LifeLogEntry
decodeLifeLogEntry =
  decode LifeLogEntry
  |> required "event" decodeLifeLogEvent
  |> required "date" decodeDate



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

decodeLifeLogEvent : Decoder LifeLogEvent
decodeLifeLogEvent =
  let
    innerDecode tag =
      case tag of
        "GaveBirth" -> Decode.succeed GaveBirth
        "Marriage" -> Decode.succeed Marriage
        "Pregnant" -> Decode.succeed Pregnant
        _ -> Decode.fail (tag ++ " is not a recognized tag for Sex")
  in
    andThen innerDecode Decode.string

-- Encoders --

encodeModel : Model -> Encode.Value
encodeModel model =
  Encode.object
    [ ("date", encodeDate model.date)
    , ("people", Encode.list (List.map encodeIndividual model.people))
    , ("peopleLastId", Encode.int model.peopleLastId)
    ]

encodeIndividual : Individual -> Encode.Value
encodeIndividual person =
  Encode.object
    [
      ("id", Encode.int person.id),
      ("genome", Encode.list <| List.map Encode.int person.genome),
      ("bornAt", encodeDate person.bornAt),
      ("sex", encodeSex person.sex),
      ("father", Encode.int person.father),
      ("mother", Encode.int person.mother),
      ("spouse", (EncodeExtra.maybe Encode.int) person.spouse),
      ("fullness", Encode.float person.fullness),
      ("pregnantAt", (EncodeExtra.maybe encodeDate) person.pregnantAt),
      ("lifeLog", Encode.list <| List.map encodeLifeLog person.lifeLog)
    ]

encodeDate : Date -> Encode.Value
encodeDate date =
  Encode.object
    [
      ("year", Encode.int date.year),
      ("day", Encode.int date.day)
    ]

encodeLifeLog : LifeLogEntry -> Encode.Value
encodeLifeLog log =
  Encode.object
    [
      ("event", encodeLifeLogEvent log.event),
      ("date", encodeDate log.date)
    ]

encodeSex : Sex -> Encode.Value
encodeSex sex =
  Encode.string <| toString sex

encodeLifeLogEvent : LifeLogEvent -> Encode.Value
encodeLifeLogEvent event =
  Encode.string <| toString event
