module Bunny.Charts.Indicator
  ( Series, sma, ema, rsi, MACD (..), macd
  , Bollinger (..), bollinger, obv
  , KDJ (..), kdj
  , SAR (..), sar
  , DMI (..), dmi
  , VR (..), vr
  , Ichimoku (..), ichimoku
  , WR (..), wr
  , PSY (..), psy
  , BIAS (..), bias
  , AVP (..), avp, AO (..), ao, BBI (..), bbi, BRAR (..), brar
  , CCI (..), cci, CR (..), cr, DMA (..), dma, EMV (..), emv
  , MTM (..), mtm, PVT (..), pvt, ROC (..), roc, TRIX (..), trix
  ) where

import Bunny.Charts (KLine (..))
import Data.List (zip5)

type Series = [Maybe Double]

validPeriod :: Int -> Bool
validPeriod p = p > 0

sma :: Int -> [KLine] -> Series
sma p xs
  | not (validPeriod p) = replicate (length xs) Nothing
  | otherwise = zipWith value [1 :: Int ..] (drop 1 sums)
  where
    values = map close xs
    sums = scanl (+) 0 values
    value i total
      | i < p = Nothing
      | otherwise = Just ((total - sums !! (i - p)) / fromIntegral p)

ema :: Int -> [KLine] -> Series
ema p xs
  | not (validPeriod p) = replicate (length xs) Nothing
  | otherwise = go 0 Nothing (map close xs)
  where
    alpha = 2 / fromIntegral (p + 1)
    go _ _ [] = []
    go total previous (x:rest)
      | count < p = Nothing : go (total + x) Nothing rest
      | count == p = Just initial : go (total + x) (Just initial) rest
      | otherwise = Just next : go (total + x) (Just next) rest
      where
        count = length xs - length rest
        initial = (total + x) / fromIntegral p
        next = alpha * x + (1 - alpha) * maybe initial id previous

rsi :: Int -> [KLine] -> Series
rsi p xs
  | not (validPeriod p) = replicate (length xs) Nothing
  | otherwise = Nothing : go (zip (map close xs) (drop 1 (map close xs))) [] []
  where
    go [] _ _ = []
    go ((before, now):rest) gains losses =
      let change = now - before
          gs = take p (max change 0 : gains)
          ls = take p (max (-change) 0 : losses)
          value | length gs < p = Nothing
                | loss == 0 = Just (if gain == 0 then 50 else 100)
                | otherwise = Just (100 - 100 / (1 + gain / loss))
          gain = sum gs / fromIntegral p
          loss = sum ls / fromIntegral p
      in value : go rest gs ls

data MACD = MACD { dif :: !Double, dea :: !Double, histogram :: !Double } deriving (Eq, Show)

macd :: Int -> Int -> Int -> [KLine] -> [Maybe MACD]
macd short long signal xs
  | any (not . validPeriod) [short, long, signal] = replicate (length xs) Nothing
  | otherwise = zipWith3 result shortE longE signalE
  where
    shortE = ema short xs
    longE = ema long xs
    difs = zipWith sub shortE longE
    signalInput = zipWith (\k d -> k { close = maybe 0 id d }) xs difs
    signalE = ema signal signalInput
    sub (Just a) (Just b) = Just (a - b)
    sub _ _ = Nothing
    result (Just d) (Just _) (Just s) = Just (MACD d s ((d - s) * 2))
    result _ _ _ = Nothing

data Bollinger = Bollinger { upper :: !Double, middle :: !Double, lower :: !Double } deriving (Eq, Show)

bollinger :: Int -> Double -> [KLine] -> [Maybe Bollinger]
bollinger p deviations xs
  | not (validPeriod p) || deviations < 0 = replicate (length xs) Nothing
  | otherwise = zipWith calculate [1 :: Int ..] (inits (map close xs))
  where
    inits values = [take i values | i <- [1 .. length values]]
    calculate i values
      | i < p = Nothing
      | otherwise = Just (Bollinger (mean + deviations * sd) mean (mean - deviations * sd))
      where
        sample = drop (i - p) values
        mean = sum sample / fromIntegral p
        sd = sqrt (sum [(v - mean) ^ (2 :: Int) | v <- sample] / fromIntegral p)

obv :: Int -> [KLine] -> [(Double, Maybe Double)]
obv p xs = zip values (movingAverage p values)
  where
    values = case xs of
      [] -> []
      first:rest -> scanl step 0 (zip rest (map close (first:rest)))
    step total (k, previous)
      | close k > previous = total + maybe 0 id (volume k)
      | close k < previous = total - maybe 0 id (volume k)
      | otherwise = total
    movingAverage n ys
      | not (validPeriod n) = replicate (length ys) Nothing
      | otherwise = [if i < n then Nothing else Just (sum (take n (drop (i - n) ys)) / fromIntegral n) | i <- [1 .. length ys]]

data KDJ = KDJ { kdjK :: !Double, kdjD :: !Double, kdjJ :: !Double } deriving (Eq, Show)

kdj :: Int -> Int -> Int -> [KLine] -> [Maybe KDJ]
kdj pK pD _ xs
  | any (not . validPeriod) [pK, pD] = replicate (length xs) Nothing
  | otherwise = go 0 50.0 50.0 xs
  where
    go _ _ _ [] = []
    go idx prevK prevD (k:rest)
      | idx < pK - 1 = Nothing : go (idx + 1) prevK prevD rest
      | otherwise =
          let window = take pK (drop (idx - pK + 1) xs)
              hN = maximum (map high window)
              lN = minimum (map low window)
              spanH = if hN == lN then 1.0 else hN - lN
              rsv = ((close k - lN) / spanH) * 100.0
              curK = (fromIntegral (pK - 1) * prevK + rsv) / fromIntegral pK
              curD = (fromIntegral (pD - 1) * prevD + curK) / fromIntegral pD
              curJ = 3.0 * curK - 2.0 * curD
          in Just (KDJ curK curD curJ) : go (idx + 1) curK curD rest

data SAR = SAR { sarValue :: !Double, isRising :: !Bool } deriving (Eq, Show)

sar :: Double -> Double -> Double -> [KLine] -> [Maybe SAR]
sar startStep step maxStep xs
  | null xs = []
  | otherwise = case xs of
      [] -> []
      first:_ ->
        let rising0 = length xs < 2 || close (xs !! 1) >= close first
            sar0 = if rising0 then low first else high first
            ep0 = if rising0 then high first else low first
        in go (0 :: Int) rising0 sar0 ep0 (startStep / 100.0) xs
  where
    afStep = step / 100.0
    afMax = maxStep / 100.0
    go _ _ _ _ _ [] = []
    go idx r s ep af (k:rest)
      | idx == 0 = Just (SAR s r) : go 1 r s ep af rest
      | otherwise =
          let nextSar0 = s + af * (ep - s)
              (newRising, newSar, newEp, newAf)
                | r =
                    if low k <= nextSar0
                    then (False, ep, low k, startStep / 100.0)
                    else (True, nextSar0, max ep (high k), if high k > ep then min afMax (af + afStep) else af)
                | otherwise =
                    if high k >= nextSar0
                    then (True, ep, high k, startStep / 100.0)
                    else (False, nextSar0, min ep (low k), if low k < ep then min afMax (af + afStep) else af)
          in Just (SAR newSar newRising) : go (idx + 1) newRising newSar newEp newAf rest

data DMI = DMI
  { pdi :: !Double
  , mdi :: !Double
  , adx :: !(Maybe Double)
  , adxr :: !(Maybe Double)
  } deriving (Eq, Show)

data DMIState = DMIState !Double !Double !Double !Double !Double !Double ![Double] ![Maybe DMI]

dmi :: Int -> Int -> [KLine] -> [Maybe DMI]
dmi period adxrPeriod xs
  | not (validPeriod period) || not (validPeriod adxrPeriod) = replicate (length xs) Nothing
  | otherwise = reverse output
  where
    DMIState _ _ _ _ _ _ _ output = foldl' step initial (zip3 [0 :: Int ..] xs (Nothing : map Just xs))
    initial = DMIState 0 0 0 0 0 0 [] []
    step (DMIState trSum hSum lSum mtr dmp dmm dxs results) (i, k, previous) =
      DMIState nextTrSum nextHSum nextLSum nextMtr nextDmp nextDmm nextDxs (result : results)
      where
        prev = maybe k id previous
        tr = maximum [high k - low k, abs (high k - close prev), abs (close prev - low k)]
        highMove = high k - high prev
        lowMove = low prev - low k
        dmPlus = if highMove > 0 && highMove > lowMove then highMove else 0
        dmMinus = if lowMove > 0 && lowMove > highMove then lowMove else 0
        nextTrSum = trSum + tr
        nextHSum = hSum + dmPlus
        nextLSum = lSum + dmMinus
        ready = i >= period - 1
        nextMtr
          | not ready = mtr
          | i == period - 1 = nextTrSum
          | otherwise = mtr - mtr / fromIntegral period + tr
        nextDmp
          | not ready = dmp
          | i == period - 1 = nextHSum
          | otherwise = dmp - dmp / fromIntegral period + dmPlus
        nextDmm
          | not ready = dmm
          | i == period - 1 = nextLSum
          | otherwise = dmm - dmm / fromIntegral period + dmMinus
        plus = if nextMtr == 0 then 0 else nextDmp * 100 / nextMtr
        minus = if nextMtr == 0 then 0 else nextDmm * 100 / nextMtr
        dx = if plus + minus == 0 then 0 else abs (minus - plus) * 100 / (minus + plus)
        nextDxs = if ready then dxs ++ [dx] else dxs
        adxValue
          | length nextDxs < period = Nothing
          | length nextDxs == period = Just (sum nextDxs / fromIntegral period)
          | otherwise = case newestADX results of
              Just previousADX -> Just ((previousADX * fromIntegral (period - 1) + dx) / fromIntegral period)
              Nothing -> Nothing
        adxrValue = do
          currentADX <- adxValue
          oldADX <- resultADX (adxrPeriod - 1) results
          pure ((oldADX + currentADX) / 2)
        result
          | not ready = Nothing
          | otherwise = Just (DMI plus minus adxValue adxrValue)

    newestADX [] = Nothing
    newestADX (Nothing:rest) = newestADX rest
    newestADX (Just value:_) = adx value
    resultADX offset results = case drop offset results of
      Just value:_ -> adx value
      _ -> Nothing

data VR = VR { vrVal :: !Double, vrMa :: !(Maybe Double) } deriving (Eq, Show)

vr :: Int -> Int -> [KLine] -> [Maybe VR]
vr period maPeriod xs
  | not (validPeriod period) || not (validPeriod maPeriod) = replicate (length xs) Nothing
  | otherwise = zipWith make [0 :: Int ..] windows
  where
    classified = zipWith classify xs (Nothing : map Just xs)
    classify k previous
      | close k > close prev = (vol, 0, 0)
      | close k < close prev = (0, vol, 0)
      | otherwise = (0, 0, vol)
      where
        prev = maybe k id previous
        vol = maybe 0 id (volume k)
    windows = [take period (drop (max 0 (i - period + 1)) classified) | i <- [0 .. length xs - 1]]
    ratios = zipWith ratio [0 :: Int ..] windows
    ratio i values
      | i < period - 1 = Nothing
      | denominator == 0 = Just 0
      | otherwise = Just ((up + flat / 2) * 100 / denominator)
      where
        (up, down, flat) = foldl' (\(a, b, c) (x, y, z) -> (a + x, b + y, c + z)) (0, 0, 0) values
        denominator = down + flat / 2
    make i _ = case ratios !! i of
      Nothing -> Nothing
      Just value -> Just (VR value movingAverage)
      where
        available = [v | Just v <- take maPeriod (drop (max 0 (i - maPeriod + 1)) ratios)]
        movingAverage
          | length available < maPeriod = Nothing
          | otherwise = Just (sum available / fromIntegral maPeriod)

data Ichimoku = Ichimoku
  { tenkan :: !(Maybe Double)
  , kijun :: !(Maybe Double)
  , senkouA :: !(Maybe Double)
  , senkouB :: !(Maybe Double)
  , chikou :: !(Maybe Double)
  } deriving (Eq, Show)

ichimoku :: Int -> Int -> Int -> Int -> [KLine] -> [Maybe Ichimoku]
ichimoku tenkanP kijunP senkouP displacement xs
  | any (not . validPeriod) [tenkanP, kijunP, senkouP, displacement] = replicate (length xs) Nothing
  | otherwise =
      let n = length xs
          midpoint p i
            | i < p - 1 = Nothing
            | otherwise =
                let window = take p (drop (i - p + 1) xs)
                    hi = maximum (map high window)
                    lo = minimum (map low window)
                in Just ((hi + lo) / 2)
          tenkans = [midpoint tenkanP i | i <- [0 .. n - 1]]
          kijuns  = [midpoint kijunP i | i <- [0 .. n - 1]]
          senkouBs = [if i >= displacement then midpoint senkouP (i - displacement) else Nothing | i <- [0 .. n - 1]]
          senkouAs =
            [ if i < displacement then Nothing
              else case (tenkans !! (i - displacement), kijuns !! (i - displacement)) of
                     (Just t, Just k) -> Just ((t + k) / 2)
                     _ -> Nothing
            | i <- [0 .. n - 1]
            ]
          chikous =
            [ if i + displacement < n
              then Just (close (xs !! (i + displacement)))
              else Nothing
            | i <- [0 .. n - 1]
            ]
      in [ Just (Ichimoku t k sa sb ch)
         | (t, k, sa, sb, ch) <- zip5 tenkans kijuns senkouAs senkouBs chikous
         ]

data WR = WR
  { wr1 :: !(Maybe Double)
  , wr2 :: !(Maybe Double)
  , wr3 :: !(Maybe Double)
  } deriving (Eq, Show)

wr :: Int -> Int -> Int -> [KLine] -> [Maybe WR]
wr p1 p2 p3 xs
  | any (not . validPeriod) [p1, p2, p3] = replicate (length xs) Nothing
  | otherwise =
      let n = length xs
          calcP p i
            | i < p - 1 = Nothing
            | otherwise =
                let window = take p (drop (i - p + 1) xs)
                    hn = maximum (map high window)
                    ln = minimum (map low window)
                    c = close (xs !! i)
                in if hn == ln then Just 0 else Just (((c - hn) / (hn - ln)) * 100)
          wr1s = [calcP p1 i | i <- [0 .. n - 1]]
          wr2s = [calcP p2 i | i <- [0 .. n - 1]]
          wr3s = [calcP p3 i | i <- [0 .. n - 1]]
      in [ Just (WR w1 w2 w3) | (w1, w2, w3) <- zip3 wr1s wr2s wr3s ]

data PSY = PSY
  { psyValue :: !(Maybe Double)
  , psyMa :: !(Maybe Double)
  } deriving (Eq, Show)

psy :: Int -> Int -> [KLine] -> [Maybe PSY]
psy period maPeriod xs
  | not (validPeriod period) || not (validPeriod maPeriod) = replicate (length xs) Nothing
  | otherwise =
      let n = length xs
          ups = [ if i == 0 then (0 :: Int) else if close (xs !! i) > close (xs !! (i - 1)) then 1 else 0 | i <- [0 .. n - 1] ]
          psyVal i
            | i < period = Nothing
            | otherwise =
                let window = take period (drop (i - period + 1) ups)
                in Just (fromIntegral (sum window) * 100 / fromIntegral period)
          psyVals = [psyVal i | i <- [0 .. n - 1]]
          maVal i
            | i < period + maPeriod - 1 = Nothing
            | otherwise =
                let window = [v | Just v <- take maPeriod (drop (i - maPeriod + 1) psyVals)]
                in if length window == maPeriod then Just (sum window / fromIntegral maPeriod) else Nothing
      in [ Just (PSY pv mv) | (pv, mv) <- zip psyVals [maVal i | i <- [0 .. n - 1]] ]

data BIAS = BIAS
  { bias1 :: !(Maybe Double)
  , bias2 :: !(Maybe Double)
  , bias3 :: !(Maybe Double)
  } deriving (Eq, Show)

bias :: Int -> Int -> Int -> [KLine] -> [Maybe BIAS]
bias p1 p2 p3 xs
  | any (not . validPeriod) [p1, p2, p3] = replicate (length xs) Nothing
  | otherwise =
      let n = length xs
          calcB p i
            | i < p - 1 = Nothing
            | otherwise =
                let window = take p (drop (i - p + 1) xs)
                    mean = sum (map close window) / fromIntegral p
                    c = close (xs !! i)
                in if mean == 0 then Nothing else Just (((c - mean) / mean) * 100)
          b1s = [calcB p1 i | i <- [0 .. n - 1]]
          b2s = [calcB p2 i | i <- [0 .. n - 1]]
          b3s = [calcB p3 i | i <- [0 .. n - 1]]
      in [ Just (BIAS b1 b2 b3) | (b1, b2, b3) <- zip3 b1s b2s b3s ]

data AVP = AVP { avpValue :: !(Maybe Double) } deriving (Eq, Show)

avp :: [KLine] -> [AVP]
avp xs = reverse output
  where
    (_, _, output) = foldl' step (0, 0, []) xs
    step (turnoverSum, volumeSum, values) k =
      let vol = max 0 (maybe 0 id (volume k))
          price = (high k + low k + close k) / 3
          turnoverValue = maybe (price * vol) id (turnover k)
          nextVolume = volumeSum + vol
          value = if nextVolume == 0 then Nothing else Just ((turnoverSum + turnoverValue) / nextVolume)
      in (turnoverSum + turnoverValue, nextVolume, AVP value : values)

data AO = AO { aoValue :: !(Maybe Double) } deriving (Eq, Show)

ao :: Int -> Int -> [KLine] -> [AO]
ao shortPeriod longPeriod xs
  | any (not . validPeriod) [shortPeriod, longPeriod] = replicate (length xs) (AO Nothing)
  | otherwise =
      let n = length xs
          middle k = (high k + low k) / 2
          average p i
            | i < p - 1 = Nothing
            | otherwise = Just (sum (map middle (take p (drop (i - p + 1) xs))) / fromIntegral p)
      in [AO (do short <- average shortPeriod i; long <- average longPeriod i; pure (short - long)) | i <- [0 .. n - 1]]

data BBI = BBI { bbiValue :: !(Maybe Double) } deriving (Eq, Show)

bbi :: Int -> Int -> Int -> Int -> [KLine] -> [BBI]
bbi p1 p2 p3 p4 xs
  | any (not . validPeriod) [p1, p2, p3, p4] = replicate (length xs) (BBI Nothing)
  | otherwise = [BBI (meanAt i) | i <- [0 .. length xs - 1]]
  where
    periods = [p1, p2, p3, p4]
    mean p i
      | i < p - 1 = Nothing
      | otherwise = Just (sum (map close (take p (drop (i - p + 1) xs))) / fromIntegral p)
    meanAt i = do
      values <- sequence [mean p i | p <- periods]
      pure (sum values / fromIntegral (length values))

data BRAR = BRAR { brValue :: !(Maybe Double), arValue :: !(Maybe Double) } deriving (Eq, Show)

brar :: Int -> [KLine] -> [BRAR]
brar period xs
  | not (validPeriod period) = replicate (length xs) (BRAR Nothing Nothing)
  | otherwise = [calculate i | i <- [0 .. length xs - 1]]
  where
    calculate i
      | i < period - 1 = BRAR Nothing Nothing
      | otherwise =
          let window = take period (drop (i - period + 1) xs)
              prevs = Nothing : map Just (take (length window - 1) window)
              hcy = sum [high k - close (maybe k id prev) | (k, prev) <- zip window prevs]
              cyl = sum [close (maybe k id prev) - low k | (k, prev) <- zip window prevs]
              ho = sum [high k - open k | k <- window]
              ol = sum [open k - low k | k <- window]
          in BRAR (Just (if cyl == 0 then 0 else hcy * 100 / cyl)) (Just (if ol == 0 then 0 else ho * 100 / ol))

data CCI = CCI { cciValue :: !(Maybe Double) } deriving (Eq, Show)

cci :: Int -> [KLine] -> [CCI]
cci period xs
  | not (validPeriod period) = replicate (length xs) (CCI Nothing)
  | otherwise = [calculate i | i <- [0 .. length xs - 1]]
  where
    typical k = (high k + low k + close k) / 3
    calculate i
      | i < period - 1 = CCI Nothing
      | otherwise =
          let values = map typical (take period (drop (i - period + 1) xs))
              mean = sum values / fromIntegral period
              deviation = sum (map (abs . subtract mean) values) / fromIntegral period
              current = typical (xs !! i)
          in CCI (Just (if deviation == 0 then 0 else (current - mean) / deviation / 0.015))

data CR = CR { crValue :: !(Maybe Double), crMa1 :: !(Maybe Double), crMa2 :: !(Maybe Double), crMa3 :: !(Maybe Double), crMa4 :: !(Maybe Double) } deriving (Eq, Show)

cr :: Int -> [Int] -> [KLine] -> [CR]
cr period maPeriods xs
  | not (validPeriod period) || any (not . validPeriod) maPeriods = replicate (length xs) empty
  | otherwise = zipWith make [0 ..] raw
  where
    empty = CR Nothing Nothing Nothing Nothing Nothing
    raw = [valueAt i | i <- [0 .. length xs - 1]]
    valueAt i
      | i < period = Nothing
      | otherwise =
          let indices = [i - period + 1 .. i]
              pair j = (xs !! j, xs !! (j - 1))
              ups = sum [max 0 (high k - (high previous + low previous) / 2) | j <- indices, let (k, previous) = pair j]
              downs = sum [max 0 ((high previous + low previous) / 2 - low k) | j <- indices, let (k, previous) = pair j]
          in Just (if downs == 0 then 0 else ups * 100 / downs)
    moving p i =
      let shift = ceiling (fromIntegral p / 2.5 + 1 :: Double)
          end = i - shift
          values = [v | Just v <- take p (drop (end - p + 1) raw)]
      in if end >= p - 1 && length values == p then Just (sum values / fromIntegral p) else Nothing
    at n i = if n < length maPeriods then moving (maPeriods !! n) i else Nothing
    make i value = CR value (at 0 i) (at 1 i) (at 2 i) (at 3 i)

data DMA = DMA { dmaValue :: !(Maybe Double), amaValue :: !(Maybe Double) } deriving (Eq, Show)

dma :: Int -> Int -> Int -> [KLine] -> [DMA]
dma shortPeriod longPeriod signalPeriod xs
  | any (not . validPeriod) [shortPeriod, longPeriod, signalPeriod] = replicate (length xs) (DMA Nothing Nothing)
  | otherwise = [DMA value (signal i) | (i, value) <- zip [0 ..] values]
  where
    average p i
      | i < p - 1 = Nothing
      | otherwise = Just (sum (map close (take p (drop (i - p + 1) xs))) / fromIntegral p)
    values = [do a <- average shortPeriod i; b <- average longPeriod i; pure (a - b) | i <- [0 .. length xs - 1]]
    signal i = let values' = [v | Just v <- take signalPeriod (drop (i - signalPeriod + 1) values)]
               in if length values' == signalPeriod then Just (sum values' / fromIntegral signalPeriod) else Nothing

data EMV = EMV { emvValue :: !(Maybe Double), emvMa :: !(Maybe Double) } deriving (Eq, Show)

emv :: Int -> Int -> [KLine] -> [EMV]
emv period signalPeriod xs
  | any (not . validPeriod) [period, signalPeriod] = replicate (length xs) (EMV Nothing Nothing)
  | otherwise = [EMV (at i) (signal i) | i <- [0 .. length xs - 1]]
  where
    raw i
      | i == 0 = Nothing
      | volume (xs !! i) <= Just 0 = Nothing
      | high k == low k = Nothing
      | otherwise = Just (((((high k + low k) - high previous - low previous) / 2) * (high k - low k) / maybe 1 id (volume k)) * 100000000)
      where k = xs !! i; previous = xs !! (i - 1)
    at i = let values = [v | Just v <- take period (drop (i - period + 1) (map raw [0 .. length xs - 1]))]
           in if length values == period then Just (sum values / fromIntegral period) else Nothing
    series = [at i | i <- [0 .. length xs - 1]]
    signal i = let values = [v | Just v <- take signalPeriod (drop (i - signalPeriod + 1) series)]
               in if length values == signalPeriod then Just (sum values / fromIntegral signalPeriod) else Nothing

data MTM = MTM { mtmValue :: !(Maybe Double), mtmMa :: !(Maybe Double) } deriving (Eq, Show)

mtm :: Int -> Int -> [KLine] -> [MTM]
mtm period signalPeriod xs
  | any (not . validPeriod) [period, signalPeriod] = replicate (length xs) (MTM Nothing Nothing)
  | otherwise = [MTM value (signal i) | (i, value) <- zip [0 ..] values]
  where
    values = [if i < period then Nothing else Just (close (xs !! i) - close (xs !! (i - period))) | i <- [0 .. length xs - 1]]
    signal i = let values' = [v | Just v <- take signalPeriod (drop (i - signalPeriod + 1) values)]
               in if length values' == signalPeriod then Just (sum values' / fromIntegral signalPeriod) else Nothing

data PVT = PVT { pvtValue :: !Double } deriving (Eq, Show)

pvt :: [KLine] -> [PVT]
pvt xs = reverse output
  where
    (_, output) = foldl' step (Nothing, []) xs
    step (previous, values) k =
      let total = case (previous, values) of
            (Just prior, PVT running:_) | prior /= 0 -> running + ((close k - prior) / prior) * maybe 0 id (volume k)
            _ -> 0
      in (Just (close k), PVT total : values)

data ROC = ROC { rocValue :: !(Maybe Double), rocMa :: !(Maybe Double) } deriving (Eq, Show)

roc :: Int -> Int -> [KLine] -> [ROC]
roc period signalPeriod xs
  | any (not . validPeriod) [period, signalPeriod] = replicate (length xs) (ROC Nothing Nothing)
  | otherwise = [ROC value (signal i) | (i, value) <- zip [0 ..] values]
  where
    values = [if i < period then Nothing else let old = close (xs !! (i - period)) in Just (if old == 0 then 0 else (close (xs !! i) - old) * 100 / old) | i <- [0 .. length xs - 1]]
    signal i = let values' = [v | Just v <- take signalPeriod (drop (i - signalPeriod + 1) values)]
               in if length values' == signalPeriod then Just (sum values' / fromIntegral signalPeriod) else Nothing

data TRIX = TRIX { trixValue :: !(Maybe Double), trixMa :: !(Maybe Double) } deriving (Eq, Show)

trix :: Int -> Int -> [KLine] -> [TRIX]
trix period signalPeriod xs
  | any (not . validPeriod) [period, signalPeriod] = replicate (length xs) (TRIX Nothing Nothing)
  | otherwise = [TRIX value (signal i) | (i, value) <- zip [0 ..] values]
  where
    emaSeries p input = go 0 Nothing input
      where
        alpha = 2 / fromIntegral (p + 1)
        go _ _ [] = []
        go total previous (x:rest)
          | count < p = Nothing : go (total + x) Nothing rest
          | count == p = Just initial : go (total + x) (Just initial) rest
          | otherwise = Just next : go (total + x) (Just next) rest
          where
            count = length input - length rest
            initial = (total + x) / fromIntegral p
            next = alpha * x + (1 - alpha) * maybe initial id previous
    e1 = emaSeries period (map close xs)
    e2 = emaSeries period [maybe 0 id v | v <- e1]
    e3 = emaSeries period [maybe 0 id v | v <- e2]
    values = zipWith previousChange (Nothing : e3) e3
    previousChange (Just previous) (Just current) | previous /= 0 = Just ((current - previous) * 100 / previous)
    previousChange _ _ = Nothing
    signal i = let values' = [v | Just v <- take signalPeriod (drop (i - signalPeriod + 1) values)]
               in if length values' == signalPeriod then Just (sum values' / fromIntegral signalPeriod) else Nothing
