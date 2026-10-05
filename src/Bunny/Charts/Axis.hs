module Bunny.Charts.Axis
  ( Tick (..)
  , linearTicks
  , priceTicks
  , indexTicks
  ) where

import Bunny.Charts.Geometry

data Tick a = Tick
  { tickValue :: !a
  , tickPosition :: !Double
  } deriving (Eq, Show)

linearTicks :: Int -> (Double, Double) -> [Double]
linearTicks count (lo, hi)
  | count < 2 = []
  | hi <= lo = [lo]
  | otherwise = [lo + fromIntegral i * step | i <- [0 .. count - 1]]
  where step = (hi - lo) / fromIntegral (count - 1)

priceTicks :: Int -> (Double, Double) -> Double -> [Tick Double]
priceTicks count range height =
  let tx = yAxisTransform range height
  in [Tick value (mapY tx value) | value <- linearTicks count range]

indexTicks :: Int -> Int -> Int -> Double -> [Tick Int]
indexTicks count first total step
  | count < 1 || total < 1 = []
  | count == 1 = [Tick first 0]
  | otherwise = [Tick index (fromIntegral (index - first) * step) | index <- indices]
  where
    lastIndex = total - 1
    spanSize = max 0 (lastIndex - first)
    indices = [first + round ((fromIntegral i * fromIntegral spanSize / fromIntegral (count - 1)) :: Double) | i <- [0 .. count - 1]]
