module Bunny.Charts.Geometry
  ( Point (..)
  , Rect (..)
  , Transform (..)
  , mapX
  , mapY
  , yAxisTransform
  , xAxisTransform
  ) where

data Point = Point !Double !Double deriving (Eq, Show)
data Rect = Rect { rectX :: !Double, rectY :: !Double, rectW :: !Double, rectH :: !Double } deriving (Eq, Show)

data Transform = Transform
  { scale :: !Double
  , offset :: !Double
  } deriving (Eq, Show)

mapX :: Transform -> Double -> Double
mapX (Transform s o) x = x * s + o

mapY :: Transform -> Double -> Double
mapY (Transform s o) y = o - y * s -- Y axis typically grows downwards in UI, so inverted

-- | Create a transform from price to Y pixels.
-- range: (minPrice, maxPrice), viewH: height of the drawing area
yAxisTransform :: (Double, Double) -> Double -> Transform
yAxisTransform (minVal, maxVal) viewH
  | maxVal <= minVal = Transform 1 (viewH / 2)
  | otherwise = Transform s o
  where
    s = viewH / (maxVal - minVal)
    o = maxVal * s

-- | Create a transform from data index to X pixels.
-- width of chart, candle bar space, start index
xAxisTransform :: Double -> Int -> Transform
xAxisTransform space startIdx = Transform space (- fromIntegral startIdx * space)
