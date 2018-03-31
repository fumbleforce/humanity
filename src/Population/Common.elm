module Population.Common exposing (..)


peopleConfig =
  { personW = 1
  , personH = 1
  , eatPerDay = 1
  , maxAge = 1
  , adultAge = 0.1
  }

isChild p =
  p.age < peopleConfig.adultAge

isAdult p =
  p.age >= peopleConfig.adultAge
