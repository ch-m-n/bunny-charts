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
  let hPrims = renderOverlay (xAxisTransform 10 0) txY (Rect 0 0 100 200) (HorizontalStraightLine 50.0 (Color 255 0 0 1.0) 1.0)
  assert "overlay hLine" (length hPrims == 1)
  let vPrims = renderOverlay (xAxisTransform 10 0) txY (Rect 0 0 100 200) (VerticalStraightLine 2 (Color 255 0 0 1.0) 1.0)
  assert "overlay vLine" (length vPrims == 1)
  let fibPrims = renderOverlay (xAxisTransform 10 0) txY (Rect 0 0 100 200) (FibonacciLine (Point2D 1 100) (Point2D 5 0) (Color 255 0 0 1.0) 1.0)
  assert "overlay fibonacci" (length fibPrims == 7)
  let hRayPrims = renderOverlay (xAxisTransform 10 0) txY (Rect 0 0 100 200) (HorizontalRayLine (Point2D 1 50) (Point2D 5 50) (Color 255 0 0 1.0) 1.0)
  assert "overlay hRay" (length hRayPrims == 1)
  let vRayPrims = renderOverlay (xAxisTransform 10 0) txY (Rect 0 0 100 200) (VerticalRayLine (Point2D 1 50) (Point2D 1 100) (Color 255 0 0 1.0) 1.0)
  assert "overlay vRay" (length vRayPrims == 1)
  let hSegPrims = renderOverlay (xAxisTransform 10 0) txY (Rect 0 0 100 200) (HorizontalSegment (Point2D 1 50) (Point2D 5 50) (Color 255 0 0 1.0) 1.0)
  assert "overlay hSeg" (length hSegPrims == 1)
  let vSegPrims = renderOverlay (xAxisTransform 10 0) txY (Rect 0 0 100 200) (VerticalSegment (Point2D 1 50) (Point2D 1 100) (Color 255 0 0 1.0) 1.0)
  assert "overlay vSeg" (length vSegPrims == 1)
  let parPrims = renderOverlay (xAxisTransform 10 0) txY (Rect 0 0 100 200) (ParallelStraightLine (Point2D 1 10) (Point2D 5 50) (Point2D 1 20) (Color 255 0 0 1.0) 1.0)
  assert "overlay parallel" (length parPrims == 2)
  let chanPrims = renderOverlay (xAxisTransform 10 0) txY (Rect 0 0 100 200) (PriceChannelLine (Point2D 1 10) (Point2D 5 50) (Point2D 1 20) (Color 255 0 0 1.0) 1.0)
  assert "overlay channel" (length chanPrims == 3)
  let annotPrims = renderOverlay (xAxisTransform 10 0) txY (Rect 0 0 100 200) (SimpleAnnotation (Point2D 1 50) "Test" (Color 255 0 0 1.0) 1.0)
  assert "overlay annotation" (length annotPrims == 2)
  let tagPrims = renderOverlay (xAxisTransform 10 0) txY (Rect 0 0 100 200) (SimpleTag 50.0 "Tag" (Color 255 0 0 1.0) 1.0)
  assert "overlay tag" (length tagPrims == 2)
  let alertPrims = renderOverlay (xAxisTransform 10 0) txY (Rect 0 0 100 200) (AlertLine 50.0 "Alert" (Color 255 0 0 1.0) 1.0)
  assert "overlay alert" (length alertPrims == 2)
  let longPrims = renderOverlay (xAxisTransform 10 0) txY (Rect 0 0 100 200) (LongPosition (Point2D 1 50) (Point2D 5 80) (Just (Point2D 5 40)) (Color 255 0 0 1.0) 1.0)
  assert "overlay longPos" (length longPrims == 5)
  let shortPrims = renderOverlay (xAxisTransform 10 0) txY (Rect 0 0 100 200) (ShortPosition (Point2D 1 50) (Point2D 5 20) (Just (Point2D 5 60)) (Color 255 0 0 1.0) 1.0)
  assert "overlay shortPos" (length shortPrims == 5)


  let tip = formatCandleTooltip candle Nothing
  assert "tooltip" (tipClose tip == "12.00")
  assert "kdj" (case kdj 1 3 3 [candle] of [Just _] -> True; _ -> False)
  assert "sar" (case sar 2 2 20 [candle] of [Just _] -> True; _ -> False)
  let trend = [KLine (fromIntegral i) (10 + fromIntegral i) (12 + fromIntegral i) (9 + fromIntegral i) (11 + fromIntegral i) (Just 10) Nothing | i <- [0 .. 7 :: Int]]
  assert "dmi warmup" (take 2 (dmi 3 2 trend) == [Nothing, Nothing])
  assert "dmi values" (case last (dmi 3 2 trend) of Just value -> pdi value > 0 && mdi value == 0 && adx value == Just 100 && adxr value == Just 100; _ -> False)
  let vrCandles = [candle, candle { timestamp = 2, close = 13 }, candle { timestamp = 3, close = 11 }, candle { timestamp = 4, close = 12 }]
  assert "vr" (case vr 3 2 vrCandles of [Nothing, Nothing, Just firstVr, Just secondVr] -> vrVal firstVr == 100 && vrVal secondVr == 200 && vrMa secondVr == Just 150; _ -> False)
  let ichiCandles = [candle { timestamp = fromIntegral i, high = 10 + fromIntegral i, low = 5 + fromIntegral i, close = 8 + fromIntegral i } | i <- [0 .. 15 :: Int]]
  assert "ichimoku" (case ichimoku 9 9 9 2 ichiCandles of res -> length res == 16)
  assert "wr" (case wr 2 3 4 [candle, candle { close = 14 }] of [Just first, Just second] -> wr1 first == Nothing && wr1 second == Just 0; _ -> False)
  assert "psy" (case psy 2 2 vrCandles of res -> length res == 4)
  assert "bias" (case bias 2 3 4 [candle, candle { close = 14 }] of [Just b0, Just b1] -> bias1 b0 == Nothing && bias1 b1 == Just ((14 - 13) / 13 * 100); _ -> False)
  assert "avp" (case avp [candle, candle { close = 14 }] of [AVP (Just v1), AVP (Just v2)] -> v1 > 0 && v2 > 0; _ -> False)
  assert "ao" (case ao 2 3 [candle, candle, candle] of [AO Nothing, AO Nothing, AO (Just _)] -> True; _ -> False)
  assert "bbi" (case bbi 2 2 2 2 [candle, candle { close = 14 }] of [BBI Nothing, BBI (Just v)] -> v == 13; _ -> False)
  assert "brar" (case brar 2 [candle, candle { close = 14 }] of [BRAR Nothing Nothing, BRAR (Just _) (Just _)] -> True; _ -> False)
  assert "cci" (case cci 2 [candle, candle { close = 14 }] of [CCI Nothing, CCI (Just _)] -> True; _ -> False)
  assert "cr" (case cr 2 [2] [candle, candle, candle] of [CR Nothing Nothing Nothing Nothing Nothing, CR Nothing Nothing Nothing Nothing Nothing, CR (Just _) _ _ _ _] -> True; _ -> False)
  assert "dma" (case dma 2 2 1 [candle, candle { close = 14 }] of [DMA Nothing Nothing, DMA (Just 0) (Just 0)] -> True; _ -> False)
  assert "emv" (case emv 1 1 [candle, candle { close = 14 }] of [EMV Nothing Nothing, EMV (Just _) (Just _)] -> True; _ -> False)
  assert "mtm" (case mtm 1 1 [candle, candle { close = 14 }] of [MTM Nothing Nothing, MTM (Just 2) (Just 2)] -> True; _ -> False)
  assert "pvt" (case pvt [candle, candle { close = 14 }] of [PVT _, PVT _] -> True; _ -> False)
  assert "roc" (case roc 1 1 [candle, candle { close = 14 }] of [ROC Nothing Nothing, ROC (Just _) (Just _)] -> True; _ -> False)
  assert "trix" (case trix 1 1 [candle, candle { close = 14 }] of [TRIX Nothing Nothing, TRIX (Just _) (Just _)] -> True; _ -> False)
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
