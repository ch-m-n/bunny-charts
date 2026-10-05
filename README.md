# bunny-charts

UI-toolkit-neutral Haskell K-line chart engine. Core depends only on `base`; use it from Monomer, GTK, SDL, Gloss, Brick, web frontends, or custom renderers.

## Design

`bunny-charts` emits plain `Primitive` values instead of importing a UI framework. Applications translate four operations—line, filled rectangle, stroked rectangle, and text—through `RenderBackend`.

```haskell
import Bunny.Charts
import Bunny.Charts.Backend
import Bunny.Charts.Geometry
import Bunny.Charts.Render

candles :: [KLine]
candles = [KLine 1 10 14 8 12 (Just 20) Nothing]

commands :: [Primitive]
commands =
  renderCandles
    defaultStyle
    (xAxisTransform 10 0)
    (yAxisTransform (8, 14) 400)
    (barSpace 800 80 2)
    candles

-- Connect toolkit drawing functions once.
backend :: RenderBackend
backend = RenderBackend
  { backendLine = \p1 p2 color width -> undefined
  , backendFillRect = \rect color -> undefined
  , backendStrokeRect = \rect color width -> undefined
  , backendText = \point text color size -> undefined
  }

main :: IO ()
main = renderPrimitives backend commands
```

Toolkit event handlers call pure functions from `Bunny.Charts.Interaction` for pan, zoom, and crosshair state. No event-loop assumptions exist.

## Modules

- `Bunny.Charts`: OHLCV data and range calculations
- `Bunny.Charts.Geometry`: coordinate transforms
- `Bunny.Charts.Render`: toolkit-neutral drawing primitives
- `Bunny.Charts.Backend`: generic renderer adapter
- `Bunny.Charts.Interaction`: pan, zoom, crosshair calculations
- `Bunny.Charts.Axis`: axis ticks
- `Bunny.Charts.Indicator`: SMA, EMA, RSI, MACD, Bollinger Bands, OBV
- `Bunny.Charts.Overlay`: drawing overlays
- `Bunny.Charts.Pane`: multi-pane layout
- `Bunny.Charts.Tooltip`: candle tooltip formatting
- `Bunny.Charts.Monomer`: compatibility adapter; generic projects should use `Backend`

## Build and check

```sh
cabal build all
cabal run bunny-charts-check
```

## Port status

Reusable engine works without GUI dependencies. Full feature parity with KLineChart is not complete: remaining indicators, overlay editing, time-axis formatting, data-source orchestration, and toolkit-specific widgets remain.
