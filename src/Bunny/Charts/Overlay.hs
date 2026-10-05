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
  | HorizontalStraightLine !Double !Color !Double
  | VerticalStraightLine !Int !Color !Double
  | FibonacciLine !Point2D !Point2D !Color !Double
  | HorizontalRayLine !Point2D !Point2D !Color !Double
  | VerticalRayLine !Point2D !Point2D !Color !Double
  | HorizontalSegment !Point2D !Point2D !Color !Double
  | VerticalSegment !Point2D !Point2D !Color !Double
  | ParallelStraightLine !Point2D !Point2D !Point2D !Color !Double
  | PriceChannelLine !Point2D !Point2D !Point2D !Color !Double
  | SimpleAnnotation !Point2D !String !Color !Double
  | SimpleTag !Double !String !Color !Double
  | AlertLine !Double !String !Color !Double
  | LongPosition !Point2D !Point2D !(Maybe Point2D) !Color !Double
  | ShortPosition !Point2D !Point2D !(Maybe Point2D) !Color !Double
  deriving (Eq, Show)

moveOverlayAnchor :: Int -> Point2D -> Overlay -> Overlay
moveOverlayAnchor anchor newPt ov = case (anchor, ov) of
  (0, StraightLine _ p2 c w)         -> StraightLine newPt p2 c w
  (1, StraightLine p1 _ c w)         -> StraightLine p1 newPt c w
  (0, Segment _ p2 c w)              -> Segment newPt p2 c w
  (1, Segment p1 _ c w)              -> Segment p1 newPt c w
  (0, RayLine _ p2 c w)              -> RayLine newPt p2 c w
  (1, RayLine p1 _ c w)              -> RayLine p1 newPt c w
  (_, PriceLine _ c w)               -> let Point2D _ p = newPt in PriceLine p c w
  (_, HorizontalStraightLine _ c w)  -> let Point2D _ p = newPt in HorizontalStraightLine p c w
  (_, VerticalStraightLine _ c w)    -> let Point2D i _ = newPt in VerticalStraightLine i c w
  (0, FibonacciLine _ p2 c w)        -> FibonacciLine newPt p2 c w
  (1, FibonacciLine p1 _ c w)        -> FibonacciLine p1 newPt c w
  (0, HorizontalRayLine _ p2 c w)    -> HorizontalRayLine newPt p2 c w
  (1, HorizontalRayLine p1 _ c w)    -> HorizontalRayLine p1 newPt c w
  (0, VerticalRayLine _ p2 c w)      -> VerticalRayLine newPt p2 c w
  (1, VerticalRayLine p1 _ c w)      -> VerticalRayLine p1 newPt c w
  (0, HorizontalSegment _ p2 c w)    -> HorizontalSegment newPt p2 c w
  (1, HorizontalSegment p1 _ c w)    -> HorizontalSegment p1 newPt c w
  (0, VerticalSegment _ p2 c w)      -> VerticalSegment newPt p2 c w
  (1, VerticalSegment p1 _ c w)      -> VerticalSegment p1 newPt c w
  (0, ParallelStraightLine _ p2 p3 c w) -> ParallelStraightLine newPt p2 p3 c w
  (1, ParallelStraightLine p1 _ p3 c w) -> ParallelStraightLine p1 newPt p3 c w
  (2, ParallelStraightLine p1 p2 _ c w) -> ParallelStraightLine p1 p2 newPt c w
  (0, PriceChannelLine _ p2 p3 c w) -> PriceChannelLine newPt p2 p3 c w
  (1, PriceChannelLine p1 _ p3 c w) -> PriceChannelLine p1 newPt p3 c w
  (2, PriceChannelLine p1 p2 _ c w) -> PriceChannelLine p1 p2 newPt c w
  (0, SimpleAnnotation _ text c w)   -> SimpleAnnotation newPt text c w
  (_, SimpleTag _ text c w)          -> let Point2D _ p = newPt in SimpleTag p text c w
  (_, AlertLine _ text c w)          -> let Point2D _ p = newPt in AlertLine p text c w
  (0, LongPosition _ tp sl c w)      -> LongPosition newPt tp sl c w
  (1, LongPosition entry _ sl c w)   -> LongPosition entry newPt sl c w
  (2, LongPosition entry tp _ c w)    -> LongPosition entry tp (Just newPt) c w
  (0, ShortPosition _ tp sl c w)     -> ShortPosition newPt tp sl c w
  (1, ShortPosition entry _ sl c w)  -> ShortPosition entry newPt sl c w
  (2, ShortPosition entry tp _ c w)  -> ShortPosition entry tp (Just newPt) c w
  _                                  -> ov

translateOverlay :: Int -> Double -> Overlay -> Overlay
translateOverlay dIndex dPrice ov = case ov of
  StraightLine p1 p2 c w           -> StraightLine (shift p1) (shift p2) c w
  Segment p1 p2 c w                -> Segment (shift p1) (shift p2) c w
  RayLine p1 p2 c w                -> RayLine (shift p1) (shift p2) c w
  PriceLine p c w                  -> PriceLine (p + dPrice) c w
  HorizontalStraightLine p c w     -> HorizontalStraightLine (p + dPrice) c w
  VerticalStraightLine i c w       -> VerticalStraightLine (i + dIndex) c w
  FibonacciLine p1 p2 c w          -> FibonacciLine (shift p1) (shift p2) c w
  HorizontalRayLine p1 p2 c w      -> HorizontalRayLine (shift p1) (shift p2) c w
  VerticalRayLine p1 p2 c w        -> VerticalRayLine (shift p1) (shift p2) c w
  HorizontalSegment p1 p2 c w      -> HorizontalSegment (shift p1) (shift p2) c w
  VerticalSegment p1 p2 c w        -> VerticalSegment (shift p1) (shift p2) c w
  ParallelStraightLine p1 p2 p3 c w -> ParallelStraightLine (shift p1) (shift p2) (shift p3) c w
  PriceChannelLine p1 p2 p3 c w    -> PriceChannelLine (shift p1) (shift p2) (shift p3) c w
  SimpleAnnotation p text c w      -> SimpleAnnotation (shift p) text c w
  SimpleTag p text c w             -> SimpleTag (p + dPrice) text c w
  AlertLine p text c w             -> AlertLine (p + dPrice) text c w
  LongPosition entry tp sl c w     -> LongPosition (shift entry) (shift tp) (fmap shift sl) c w
  ShortPosition entry tp sl c w    -> ShortPosition (shift entry) (shift tp) (fmap shift sl) c w
  where
    shift (Point2D i p) = Point2D (i + dIndex) (p + dPrice)


renderOverlay :: Transform -> Transform -> Rect -> Overlay -> [Primitive]
renderOverlay xTx yTx (Rect vx vy vw vh) ov = case ov of
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

  HorizontalStraightLine price col w ->
    let py = mapY yTx price
    in [Line (Point vx py) (Point (vx + vw) py) col w]

  VerticalStraightLine idx col w ->
    let px = mapX xTx (fromIntegral idx)
    in [Line (Point px vy) (Point px (vy + vh)) col w]

  FibonacciLine (Point2D i1 p1) (Point2D i2 p2) col w ->
    let x1 = mapX xTx (fromIntegral i1)
        x2 = mapX xTx (fromIntegral i2)
        minX = min x1 x2
        maxX = max x1 x2
        padding = max 40.0 ((maxX - minX) * 0.1)
        startX = max vx (minX - padding)
        endX = min (vx + vw) (maxX + padding)
        levels = [1.0, 0.786, 0.618, 0.5, 0.382, 0.236, 0.0 :: Double]
        valDiff = p1 - p2
        lines' =
          [ Line (Point startX (mapY yTx (p2 + valDiff * lvl)))
                 (Point endX (mapY yTx (p2 + valDiff * lvl)))
                 col w
          | lvl <- levels
          ]
    in lines'

  HorizontalRayLine (Point2D i1 p1) (Point2D i2 _) col w ->
    let xA = mapX xTx (fromIntegral i1)
        xB = mapX xTx (fromIntegral i2)
        y = mapY yTx p1
        targetX = if xA < xB then vx + vw else vx
    in [Line (Point xA y) (Point targetX y) col w]

  VerticalRayLine (Point2D i1 p1) (Point2D _ p2) col w ->
    let x = mapX xTx (fromIntegral i1)
        yA = mapY yTx p1
        yB = mapY yTx p2
        targetY = if yA < yB then vy + vh else vy
    in [Line (Point x yA) (Point x targetY) col w]

  HorizontalSegment (Point2D i1 p1) (Point2D i2 _) col w ->
    let xA = mapX xTx (fromIntegral i1)
        xB = mapX xTx (fromIntegral i2)
        y = mapY yTx p1
    in [Line (Point xA y) (Point xB y) col w]

  VerticalSegment (Point2D i1 p1) (Point2D _ p2) col w ->
    let x = mapX xTx (fromIntegral i1)
        yA = mapY yTx p1
        yB = mapY yTx p2
    in [Line (Point x yA) (Point x yB) col w]

  ParallelStraightLine (Point2D i1 p1) (Point2D i2 p2) (Point2D i3 p3) col w ->
    let xA = mapX xTx (fromIntegral i1)
        yA = mapY yTx p1
        xB = mapX xTx (fromIntegral i2)
        yB = mapY yTx p2
        xC = mapX xTx (fromIntegral i3)
        yC = mapY yTx p3
        k = if xA == xB then 0 else (yB - yA) / (xB - xA)
        b1 = yA - k * xA
        b2 = yC - k * xC
    in [ Line (Point vx (k * vx + b1)) (Point (vx + vw) (k * (vx + vw) + b1)) col w
       , Line (Point vx (k * vx + b2)) (Point (vx + vw) (k * (vx + vw) + b2)) col w
       ]

  PriceChannelLine (Point2D i1 p1) (Point2D i2 p2) (Point2D i3 p3) col w ->
    let xA = mapX xTx (fromIntegral i1)
        yA = mapY yTx p1
        xB = mapX xTx (fromIntegral i2)
        yB = mapY yTx p2
        xC = mapX xTx (fromIntegral i3)
        yC = mapY yTx p3
        k = if xA == xB then 0 else (yB - yA) / (xB - xA)
        b1 = yA - k * xA
        b2 = yC - k * xC
        b3 = b1 + (b1 - b2)
    in [ Line (Point vx (k * vx + b1)) (Point (vx + vw) (k * (vx + vw) + b1)) col w
       , Line (Point vx (k * vx + b2)) (Point (vx + vw) (k * (vx + vw) + b2)) col w
       , Line (Point vx (k * vx + b3)) (Point (vx + vw) (k * (vx + vw) + b3)) col w
       ]

  SimpleAnnotation (Point2D i p) label col w ->
    let x = mapX xTx (fromIntegral i)
        y = mapY yTx p
    in [ Line (Point x y) (Point x (y - 30)) col w
       , Text (Point x (y - 35)) label col 11
       ]

  SimpleTag p label col w ->
    let y = mapY yTx p
    in [ Line (Point vx y) (Point (vx + vw) y) col w
       , Text (Point (vx + vw - 10) y) label col 11
       ]

  AlertLine p label col w ->
    let y = mapY yTx p
    in [ Line (Point vx y) (Point (vx + vw) y) col w
       , Text (Point (vx + vw / 2) y) label col 11
       ]

  LongPosition (Point2D i1 entry) (Point2D i2 tp) mSl col w ->
    let x1 = mapX xTx (fromIntegral i1)
        x2 = mapX xTx (fromIntegral i2)
        entryY = mapY yTx entry
        tpY = mapY yTx tp
        slY = case mSl of
          Just (Point2D _ sl) -> mapY yTx sl
          Nothing -> entryY + (entryY - tpY)
        minX = min x1 x2
        maxX = max x1 x2
        width = maxX - minX
        rectTop = min tpY entryY
        rectBot = max tpY entryY
        slTop = min entryY slY
        slBot = max entryY slY
    in [ FillRect (Rect minX rectTop width (rectBot - rectTop)) (Color 38 166 154 0.2)
       , FillRect (Rect minX slTop width (slBot - slTop)) (Color 239 83 80 0.2)
       , Line (Point minX entryY) (Point maxX entryY) col w
       , Line (Point minX tpY) (Point maxX tpY) (Color 38 166 154 1.0) w
       , Line (Point minX slY) (Point maxX slY) (Color 239 83 80 1.0) w
       ]

  ShortPosition (Point2D i1 entry) (Point2D i2 tp) mSl col w ->
    let x1 = mapX xTx (fromIntegral i1)
        x2 = mapX xTx (fromIntegral i2)
        entryY = mapY yTx entry
        tpY = mapY yTx tp
        slY = case mSl of
          Just (Point2D _ sl) -> mapY yTx sl
          Nothing -> entryY - (tpY - entryY)
        minX = min x1 x2
        maxX = max x1 x2
        width = maxX - minX
        rectTop = min tpY entryY
        rectBot = max tpY entryY
        slTop = min entryY slY
        slBot = max entryY slY
    in [ FillRect (Rect minX rectTop width (rectBot - rectTop)) (Color 38 166 154 0.2)
       , FillRect (Rect minX slTop width (slBot - slTop)) (Color 239 83 80 0.2)
       , Line (Point minX entryY) (Point maxX entryY) col w
       , Line (Point minX tpY) (Point maxX tpY) (Color 38 166 154 1.0) w
       , Line (Point minX slY) (Point maxX slY) (Color 239 83 80 1.0) w
       ]
