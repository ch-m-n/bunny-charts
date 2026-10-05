module Bunny.Charts.Interaction
  ( Viewport (..)
  , Crosshair (..)
  , defaultViewport
  , visibleBars
  , pan
  , zoomAt
  , crosshairAt
  , clearCrosshair
  ) where

import Bunny.Charts

data Viewport = Viewport
  { firstBar :: !Int
  , barCount :: !Int
  , barWidth :: !Double
  , gapWidth :: !Double
  } deriving (Eq, Show)

data Crosshair = Crosshair
  { crosshairX :: !Double
  , crosshairY :: !Double
  , crosshairIndex :: !(Maybe Int)
  , crosshairPrice :: !(Maybe Double)
  } deriving (Eq, Show)

defaultViewport :: Viewport
defaultViewport = Viewport 0 100 8 2

visibleBars :: Viewport -> [KLine] -> [KLine]
visibleBars vp = visibleRange (firstBar vp) (barCount vp)

pan :: Int -> Int -> Viewport -> Viewport
pan delta total vp = vp { firstBar = clamp 0 (max 0 (total - barCount vp)) (firstBar vp + delta) }

zoomAt :: Double -> Double -> Int -> Viewport -> Viewport
zoomAt factor anchor total vp
  | factor <= 0 = vp
  | otherwise = vp { firstBar = newFirst, barCount = newCount }
  where
    oldCount = max 1 (barCount vp)
    newCount = clamp 1 (max 1 total) (round (fromIntegral oldCount / factor))
    ratio = clamp 0 1 (anchor / fromIntegral oldCount)
    newFirst = clamp 0 (max 0 (total - newCount))
      (round (fromIntegral (firstBar vp) + ratio * fromIntegral (oldCount - newCount)))

crosshairAt :: Viewport -> Double -> Double -> Double -> [KLine] -> Crosshair
crosshairAt vp x y width bars = Crosshair x y index price
  where
    step = barWidth vp + gapWidth vp
    raw = if step <= 0 then 0 else x / step
    local = round raw
    candidate = firstBar vp + local
    index = if x < 0 || x > width || candidate < 0 || candidate >= length bars then Nothing else Just candidate
    price = (\i -> close (bars !! i)) <$> index

clearCrosshair :: Crosshair -> Crosshair
clearCrosshair c = c { crosshairIndex = Nothing, crosshairPrice = Nothing }

clamp :: Ord a => a -> a -> a -> a
clamp lo hi = max lo . min hi
