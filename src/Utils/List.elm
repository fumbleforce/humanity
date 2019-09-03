module Utils.List exposing
  ( zipWithSeed
  , mapWithSeed
  , mapWithMemo
  , mapExpandWithMemo
  , groupBy)

import Dict exposing (Dict, empty, update)
import Random exposing (bool, step, Seed)

zipWithSeed:
  (a -> a -> Seed -> (a, Seed))
   -> Seed
   -> List a
   -> List a
   -> (List a, Seed)
zipWithSeed merger seed firstList secondList =
  case firstList of
    [] ->
      ([], seed)
    first :: [] ->
      let
        second = case secondList of
          [] -> first
          elem :: _ -> elem
        (merged, nextSeed) = merger first second seed
      in
        ([merged], nextSeed)
    first :: firstRest ->
      let
        (second, secondRest) = case secondList of
          [] -> (first, [])
          elem :: rest -> (elem, rest)
        (merged, nextSeed) = merger first second seed
        (resList, lastSeed) = zipWithSeed merger nextSeed firstRest secondRest
      in
        (merged :: resList, lastSeed)

mapWithSeed:
  (a -> Seed -> (a, Seed))
 -> Seed
 -> List a
 -> (List a, Seed)
mapWithSeed mapper seed list =
  case list of
    [] ->
      ([], seed)
    first :: [] ->
      let
        (mapped, nextSeed) = mapper first seed
      in
        ([mapped], nextSeed)
    first :: rest ->
      let
        (mapped, nextSeed) = mapper first seed
        (resList, lastSeed) = mapWithSeed mapper nextSeed rest
      in
        (mapped :: resList, lastSeed)

mapWithMemo:
  (a -> mem -> (a, mem))
 -> mem
 -> List a
 -> (List a, mem)
mapWithMemo mapper mem list =
  case list of
    [] ->
      ([], mem)
    first :: [] ->
      let
        (mapped, nextMem) = mapper first mem
      in
        ([mapped], nextMem)
    first :: rest ->
      let
        (mapped, nextMem) = mapper first mem
        (resList, lastMem) = mapWithMemo mapper nextMem rest
      in
        (mapped :: resList, lastMem)


mapExpandWithMemo:
  (mem -> a -> (List a, mem))
 -> mem
 -> List a
 -> (List a, mem)
mapExpandWithMemo mapper mem list =
  case list of
    [] ->
      ([], mem)
    first :: [] ->
      let
        (mapped, nextMem) = mapper mem first
      in
        (mapped, nextMem)
    first :: rest ->
      let
        (mapped, nextMem) = mapper mem first
        (resList, lastMem) = mapExpandWithMemo mapper nextMem rest
      in
        (mapped ++ resList, lastMem)

groupBy : (a -> comparable)-> List a -> Dict comparable (List a)
groupBy fun =
  let
    add2Maybe x m =
      case m of
        Nothing -> Just [x]
        Just xs -> Just (xs ++ [x])

    -- add2Maybe x = -- alternative implementation
    --   Just << (flip (++)) [x] << Maybe.withDefault []

    foldF e =
      update (fun e) (add2Maybe e)
  in
    List.foldl foldF empty