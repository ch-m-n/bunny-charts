module Bunny.Charts.Tooltip
  ( CandleTooltip (..)
  , formatCandleTooltip
  ) where

import Bunny.Charts
import Text.Printf (printf)

data CandleTooltip = CandleTooltip
  { tipTime :: !String
  , tipOpen :: !String
  , tipHigh :: !String
  , tipLow :: !String
  , tipClose :: !String
  , tipVolume :: !String
  , tipChange :: !String
  , tipAmplitude :: !String
  } deriving (Eq, Show)

formatCandleTooltip :: KLine -> Maybe KLine -> CandleTooltip
formatCandleTooltip current mPrev = CandleTooltip
  { tipTime = show (timestamp current)
  , tipOpen = formatPrice (open current)
  , tipHigh = formatPrice (high current)
  , tipLow = formatPrice (low current)
  , tipClose = formatPrice (close current)
  , tipVolume = maybe "-" formatPrice (volume current)
  , tipChange = formatPercent change
  , tipAmplitude = formatPercent amp
  }
  where
    prevClose = maybe (open current) close mPrev
    change = if prevClose == 0 then 0 else ((close current - prevClose) / prevClose) * 100
    amp = if low current == 0 then 0 else ((high current - low current) / low current) * 100

    formatPrice :: Double -> String
    formatPrice p = printf "%.2f" p

    formatPercent :: Double -> String
    formatPercent p = printf "%+.2f%%" p
