{-# LANGUAGE DerivingStrategies #-}

-- | Pure K-line data and geometry primitives. Monomer rendering stays in
-- -- a thin adapter, so this package has no GUI dependency.
module Bunny.Charts
  ( Timestamp
  , KLine (..)
  , BarSpace (..)
  , OHLC (..)
  , validateKLine
  , barSpace
  , visibleRange
  , priceRange
  ) where


type Timestamp = Integer

data KLine = KLine
  { timestamp :: !Timestamp
  , open :: !Double
  , high :: !Double
  , low :: !Double
  , close :: !Double
  , volume :: !(Maybe Double)
  , turnover :: !(Maybe Double)
  } deriving stock (Eq, Show)

data BarSpace = BarSpace
  { bar :: !Double
  , halfBar :: !Double
  , gapBar :: !Double
  , halfGapBar :: !Double
  } deriving stock (Eq, Show)

data OHLC = OHLC { ohlcOpen :: !Double, ohlcHigh :: !Double, ohlcLow :: !Double, ohlcClose :: !Double }
  deriving stock (Eq, Show)

validateKLine :: KLine -> Either String KLine
validateKLine k
  | high k < max (open k) (close k) = Left "high below open or close"
  | low k > min (open k) (close k) = Left "low above open or close"
  | high k < low k = Left "high below low"
  | maybe False (< 0) (volume k) = Left "negative volume"
  | otherwise = Right k

barSpace :: Double -> Int -> Double -> BarSpace
barSpace width count gap
  | count <= 0 = BarSpace 0 0 0 0
  | otherwise = BarSpace b (b / 2) (b + gap) ((b + gap) / 2)
  where b = max 0 ((width - gap * fromIntegral (count - 1)) / fromIntegral count)

visibleRange :: Int -> Int -> [KLine] -> [KLine]
visibleRange start count = take (max 0 count) . drop (max 0 start)

priceRange :: [KLine] -> Maybe (Double, Double)
priceRange [] = Nothing
priceRange (k:ks) = Just (foldl' min (low k) (map low ks), foldl' max (high k) (map high ks))
