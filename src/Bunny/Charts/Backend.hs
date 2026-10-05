-- | UI-toolkit-neutral sink for chart primitives.
-- Adapt these four callbacks to SDL, GTK, Gloss, Brick, Monomer, or custom UI.
module Bunny.Charts.Backend
  ( RenderBackend (..)
  , renderPrimitives
  ) where

import Bunny.Charts.Geometry
import Bunny.Charts.Render

data RenderBackend = RenderBackend
  { backendLine :: Point -> Point -> Color -> Double -> IO ()
  , backendFillRect :: Rect -> Color -> IO ()
  , backendStrokeRect :: Rect -> Color -> Double -> IO ()
  , backendText :: Point -> String -> Color -> Int -> IO ()
  }

renderPrimitives :: RenderBackend -> [Primitive] -> IO ()
renderPrimitives backend = mapM_ renderOne
  where
    renderOne (Line p1 p2 color width) = backendLine backend p1 p2 color width
    renderOne (FillRect rect color) = backendFillRect backend rect color
    renderOne (StrokeRect rect color width) = backendStrokeRect backend rect color width
    renderOne (Text point text color size) = backendText backend point text color size
