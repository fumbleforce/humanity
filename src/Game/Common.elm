module Game.Common exposing (
  dateToDay
  , dayToDate)

import Types exposing (Date)
import Settings exposing (settings)



dateToDay : Date -> Int
dateToDay date =
  date.year * settings.daysInYear + date.day

dayToDate : Int -> Date
dayToDate day =
  { year = day // settings.daysInYear
  , day = day % settings.daysInYear
  }
