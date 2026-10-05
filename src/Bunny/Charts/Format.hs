module Bunny.Charts.Format
  ( formatTimestamp
  , formatTimeOnly
  , formatDateOnly
  ) where

import Text.Printf (printf)

-- | Format epoch millisecond timestamp to "YYYY-MM-DD HH:MM"
formatTimestamp :: Integer -> String
formatTimestamp ms =
  let totalSecs = ms `div` 1000
      (days, remSecs) = totalSecs `divMod` 86400
      (hours, minSecs) = remSecs `divMod` 3600
      (mins, _) = minSecs `divMod` 60
      (year, month, day) = daysToCivil (days + 719468)
  in printf "%04d-%02d-%02d %02d:%02d" year month day hours mins

formatDateOnly :: Integer -> String
formatDateOnly ms =
  let totalSecs = ms `div` 1000
      days = totalSecs `div` 86400
      (year, month, day) = daysToCivil (days + 719468)
  in printf "%04d-%02d-%02d" year month day

formatTimeOnly :: Integer -> String
formatTimeOnly ms =
  let totalSecs = ms `div` 1000
      remSecs = totalSecs `rem` 86400
      (hours, minSecs) = remSecs `divMod` 3600
      (mins, _) = minSecs `divMod` 60
  in printf "%02d:%02d" hours mins

-- | Algorithm from Howard Hinnant (std::chrono date math, public domain)
daysToCivil :: Integer -> (Integer, Integer, Integer)
daysToCivil z =
  let era = (if z >= 0 then z else z - 146096) `div` 146097
      doe = z - era * 146097
      yoe = (doe - doe `div` 1460 + doe `div` 36524 - doe `div` 146096) `div` 365
      y = yoe + era * 400
      doy = doe - (365 * yoe + yoe `div` 4 - yoe `div` 100)
      mp = (5 * doy + 2) `div` 153
      d = doy - (153 * mp + 2) `div` 5 + 1
      m = if mp < 10 then mp + 3 else mp - 9
      year = if m <= 2 then y + 1 else y
  in (year, m, d)
