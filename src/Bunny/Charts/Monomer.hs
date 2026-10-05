module Bunny.Charts.Monomer
  ( MonomerCanvas (..)
  , renderPrimitives
  ) where

import Bunny.Charts.Geometry
import Bunny.Charts.Render

data MonomerCanvas = MonomerCanvas
  { drawLine :: Point -> Point -> Color -> Double -> IO ()
  , fillRect :: Rect -> Color -> IO ()
  , strokeRect :: Rect -> Color -> Double -> IO ()
  , drawText :: Point -> String -> Color -> Int -> IO ()
  }

renderPrimitives :: MonomerCanvas -> [Primitive] -> IO ()
renderPrimitives canvas = mapM_ renderOne
  where
    renderOne (Line p1 p2 col w) = drawLine canvas p1 p2 col w
    renderOne (FillRect r col) = fillRect canvas r col
    renderOne (StrokeRect r col w) = strokeRect canvas r col w
    renderOne (Text p str col s) = drawText canvas p str col s
