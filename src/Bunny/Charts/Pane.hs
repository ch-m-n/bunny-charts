module Bunny.Charts.Pane
  ( Pane (..)
  , PaneId (..)
  , PaneLayout (..)
  , calculateLayout
  ) where

import Bunny.Charts.Geometry

data PaneId = CandlePane | IndicatorPane !String deriving (Eq, Show)

data Pane = Pane
  { paneId :: !PaneId
  , paneWeight :: !Double -- relative height ratio
  , paneMinHeight :: !Double
  } deriving (Eq, Show)

data PaneLayout = PaneLayout
  { layoutPaneId :: !PaneId
  , layoutBounds :: !Rect
  } deriving (Eq, Show)

calculateLayout :: Rect -> [Pane] -> [PaneLayout]
calculateLayout (Rect x y w h) panes
  | null panes = []
  | totalWeight <= 0 = []
  | otherwise = reverse layouts
  where
    totalWeight = sum (map paneWeight panes)
    (_, layouts) = foldl allocate (y, []) panes
    allocate (currentY, acc) p =
      let paneH = max (paneMinHeight p) (h * (paneWeight p / totalWeight))
          rect = Rect x currentY w paneH
      in (currentY + paneH, PaneLayout (paneId p) rect : acc)
