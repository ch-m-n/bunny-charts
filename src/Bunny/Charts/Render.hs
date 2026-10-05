module Bunny.Charts.Render
  ( Color (..)
  , Style (..)
  , Primitive (..)
  , defaultStyle
  , renderCandles
  , renderVolume
  , renderLine
  , renderGrid
  ) where

import Bunny.Charts
import Bunny.Charts.Geometry

data Color = Color !Int !Int !Int !Double deriving (Eq, Show)

data Style = Style
  { upColor :: !Color
  , downColor :: !Color
  , noChangeColor :: !Color
  , wickWidth :: !Double
  } deriving (Eq, Show)

defaultStyle :: Style
defaultStyle = Style
  { upColor = Color 38 166 154 1.0     -- teal/green
  , downColor = Color 239 83 80 1.0    -- red
  , noChangeColor = Color 140 140 140 1.0
  , wickWidth = 1.0
  }

data Primitive
  = Line !Point !Point !Color !Double
  | FillRect !Rect !Color
  | StrokeRect !Rect !Color !Double
  | Text !Point !String !Color !Int
  deriving (Eq, Show)

renderCandles :: Style -> Transform -> Transform -> BarSpace -> [KLine] -> [Primitive]
renderCandles style xTx yTx space klines = concatMap (drawCandle style xTx yTx (bar space)) (zip [0..] klines)

drawCandle :: Style -> Transform -> Transform -> Double -> (Int, KLine) -> [Primitive]
drawCandle style xTx yTx barW (idx, k) = [wick, body]
  where
    cx = mapX xTx (fromIntegral idx)
    yO = mapY yTx (open k)
    yC = mapY yTx (close k)
    yH = mapY yTx (high k)
    yL = mapY yTx (low k)

    color
      | close k > open k = upColor style
      | close k < open k = downColor style
      | otherwise = noChangeColor style

    wick = Line (Point cx yH) (Point cx yL) color (wickWidth style)

    topY = min yO yC
    h = max 1.0 (abs (yO - yC))
    body = FillRect (Rect (cx - barW / 2) topY barW h) color

renderVolume :: Style -> Transform -> Transform -> BarSpace -> [KLine] -> [Primitive]
renderVolume style xTx yTx space klines = concatMap (drawVol style xTx yTx (bar space)) (zip [0 :: Int ..] klines)
  where
    drawVol st xT yT barWidth (idx, k) =
      case volume k of
        Nothing -> []
        Just vol ->
          let cx = mapX xT (fromIntegral idx)
              yZero = mapY yT 0.0
              yVal = mapY yT vol
              topY = min yZero yVal
              h = max 1.0 (abs (yZero - yVal))
              col = if close k >= open k then upColor st else downColor st
          in [FillRect (Rect (cx - barWidth / 2) topY barWidth h) col]

renderLine :: Color -> Double -> Transform -> Transform -> [Maybe Double] -> [Primitive]
renderLine col width xTx yTx values =
  let points = [ (idx, Point (mapX xTx (fromIntegral idx)) (mapY yTx v))
               | (idx, Just v) <- zip [0 :: Int ..] values ]
      segments (p1:p2:rest) = Line (snd p1) (snd p2) col width : segments (p2:rest)
      segments _ = []
  in segments points

renderGrid :: Rect -> Int -> Int -> Color -> [Primitive]
renderGrid (Rect x y w h) cols rows col = xLines ++ yLines
  where
    colStep = w / fromIntegral (max 1 cols)
    rowStep = h / fromIntegral (max 1 rows)

    xLines = [ Line (Point (x + i * colStep) y) (Point (x + i * colStep) (y + h)) col 1.0 | i <- [0 .. fromIntegral cols] ]
    yLines = [ Line (Point x (y + j * rowStep)) (Point (x + w) (y + j * rowStep)) col 1.0 | j <- [0 .. fromIntegral rows] ]
