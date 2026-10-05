module Main (main) where

import Bunny.Charts
import Bunny.Charts.Indicator
import Bunny.Charts.Geometry
import Bunny.Charts.Render
import Bunny.Charts.Interaction
import Bunny.Charts.Axis
import Bunny.Charts.Overlay
import Bunny.Charts.Pane
import Bunny.Charts.Tooltip
import Bunny.Charts.Format
import Bunny.Charts.Monomer
import qualified Bunny.Charts.Backend as Backend

main :: IO ()
main = do
  let candle = KLine 1 10 14 8 12 (Just 20) Nothing
  assert "valid candle" (validateKLine candle == Right candle)
  assert "range" (priceRange [candle] == Just (8, 14))
  assert "visible" (visibleRange 0 1 [candle] == [candle])
  assert "spacing" (barSpace 29 3 1 == BarSpace 9 4.5 10 5)
  assert "sma" (sma 2 [candle, candle { close = 14 }] == [Nothing, Just 13])
  assert "rsi" (rsi 1 [candle, candle { close = 14 }] == [Nothing, Just 100])
  let txY = yAxisTransform (0, 100) 200
  assert "yTransform" (mapY txY 50 == 100.0)
  let renderPrims = renderCandles defaultStyle (xAxisTransform 10 0) txY (BarSpace 8 4 10 5) [candle]
  assert "renderCandles" (length renderPrims == 2)
  let volPrims = renderVolume defaultStyle (xAxisTransform 10 0) (yAxisTransform (0, 100) 100) (BarSpace 8 4 10 5) [candle]
  assert "renderVolume" (length volPrims == 1)
  let maPrims = renderLine (Color 0 0 255 1.0) 1.0 (xAxisTransform 10 0) txY [Just 10.0, Just 20.0]
  assert "renderLine" (length maPrims == 1)
  let bars = replicate 200 candle
  assert "pan bounded" (firstBar (pan 500 200 defaultViewport) == 100)
  assert "zoom anchored" (zoomAt 2 50 200 defaultViewport == Viewport 25 50 8 2)
  assert "crosshair" (crosshairIndex (crosshairAt defaultViewport 15 20 1000 bars) == Just 2)
  assert "price ticks" (map tickPosition (priceTicks 3 (0, 100) 200) == [200, 100, 0])
  let bollVals = bollinger 2 2.0 [candle, candle { close = 14 }]
  assert "bollinger" (case bollVals of [Nothing, Just b] -> middle b == 13; _ -> False)
  let obvVals = obv 1 [candle, candle { close = 14 }]
  assert "obv" (length obvVals == 2)
  let paneLayouts = calculateLayout (Rect 0 0 100 200) [Pane CandlePane 0.8 100, Pane (IndicatorPane "VOL") 0.2 40]
  assert "paneLayout" (length paneLayouts == 2)
  let overlayPrims = renderOverlay (xAxisTransform 10 0) txY (Rect 0 0 100 200) (PriceLine 10.0 (Color 255 0 0 1.0) 1.0)
  assert "overlay" (length overlayPrims == 1)
  let tip = formatCandleTooltip candle Nothing
  assert "tooltip" (tipClose tip == "12.00")
  assert "kdj" (case kdj 1 3 3 [candle] of [Just _] -> True; _ -> False)
  assert "sar" (case sar 2 2 20 [candle] of [Just _] -> True; _ -> False)
  assert "format timestamp" (formatTimestamp 0 == "1970-01-01 00:00")
  assert "overlay edit" (moveOverlayAnchor 0 (Point2D 2 20) (Segment (Point2D 1 10) (Point2D 3 30) (Color 0 0 0 1) 1) == Segment (Point2D 2 20) (Point2D 3 30) (Color 0 0 0 1) 1)
  let dummyCanvas = MonomerCanvas (\_ _ _ _ -> pure ()) (\_ _ -> pure ()) (\_ _ _ -> pure ()) (\_ _ _ _ -> pure ())
  renderPrimitives dummyCanvas overlayPrims
  let dummyBackend = Backend.RenderBackend (\_ _ _ _ -> pure ()) (\_ _ -> pure ()) (\_ _ _ -> pure ()) (\_ _ _ _ -> pure ())
  Backend.renderPrimitives dummyBackend renderPrims

assert :: String -> Bool -> IO ()
assert name ok
  | ok = pure ()
  | otherwise = error ("failed: " <> name)
