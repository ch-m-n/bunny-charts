module Bunny.Charts.Overlay
  ( Overlay (..)
  , Point2D (..)
  , renderOverlay
  , moveOverlayAnchor
  , translateOverlay
  ) where

import Bunny.Charts.Geometry
import Bunny.Charts.Render

data Point2D = Point2D !Int !Double deriving (Eq, Show)

data Overlay
  = StraightLine !Point2D !Point2D !Color !Double
  | Segment !Point2D !Point2D !Color !Double
  | RayLine !Point2D !Point2D !Color !Double
  | PriceLine !Double !Color !Double
  deriving (Eq, Show)

moveOverlayAnchor :: Int -> Point2D -> Overlay -> Overlay
moveOverlayAnchor anchor newPt ov = case (anchor, ov) of
  (0, StraightLine _ p2 c w) -> StraightLine newPt p2 c w
  (1, StraightLine p1 _ c w) -> StraightLine p1 newPt c w
  (0, Segment _ p2 c w)      -> Segment newPt p2 c w
  (1, Segment p1 _ c w)      -> Segment p1 newPt c w
  (0, RayLine _ p2 c w)      -> RayLine newPt p2 c w
  (1, RayLine p1 _ c w)      -> RayLine p1 newPt c w
  (_, PriceLine _ c w)       -> let Point2D _ p = newPt in PriceLine p c w
  _                          -> ov

translateOverlay :: Int -> Double -> Overlay -> Overlay
translateOverlay dIndex dPrice ov = case ov of
  StraightLine p1 p2 c w -> StraightLine (shift p1) (shift p2) c w
  Segment p1 p2 c w      -> Segment (shift p1) (shift p2) c w
  RayLine p1 p2 c w      -> RayLine (shift p1) (shift p2) c w
  PriceLine p c w        -> PriceLine (p + dPrice) c w
  where
    shift (Point2D i p) = Point2D (i + dIndex) (p + dPrice)

renderOverlay :: Transform -> Transform -> Rect -> Overlay -> [Primitive]
renderOverlay xTx yTx (Rect vx _ vw _) ov = case ov of
  StraightLine (Point2D i1 p1) (Point2D i2 p2) col w ->
    let pA = Point (mapX xTx (fromIntegral i1)) (mapY yTx p1)
        pB = Point (mapX xTx (fromIntegral i2)) (mapY yTx p2)
    in [Line pA pB col w]

  Segment (Point2D i1 p1) (Point2D i2 p2) col w ->
    let pA = Point (mapX xTx (fromIntegral i1)) (mapY yTx p1)
        pB = Point (mapX xTx (fromIntegral i2)) (mapY yTx p2)
    in [Line pA pB col w]

  RayLine (Point2D i1 p1) (Point2D i2 p2) col w ->
    let pA = Point (mapX xTx (fromIntegral i1)) (mapY yTx p1)
        pB = Point (mapX xTx (fromIntegral i2)) (mapY yTx p2)
    in [Line pA pB col w]

  PriceLine price col w ->
    let py = mapY yTx price
    in [Line (Point vx py) (Point (vx + vw) py) col w]
