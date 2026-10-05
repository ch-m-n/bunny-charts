module Bunny.Charts.Indicator
  ( Series, sma, ema, rsi, MACD (..), macd
  , Bollinger (..), bollinger, obv
  , KDJ (..), kdj
  , SAR (..), sar
  , DMI (..), dmi
  , VR (..), vr
  ) where

import Bunny.Charts (KLine (..))

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

data DMI = DMI { pdi :: !Double, mdi :: !Double, adx :: !Double } deriving (Eq, Show)

dmi :: Int -> Int -> [KLine] -> [Maybe DMI]
dmi period _ xs
  | not (validPeriod period) = replicate (length xs) Nothing
  | otherwise =
      let trs = zipWith calcTR xs (Nothing : map Just xs)
          calcTR k mPrev = case mPrev of
            Nothing -> high k - low k
            Just p -> maximum [high k - low k, abs (high k - close p), abs (low k - close p)]
          smTR = scanl (+) 0 trs
      in [ if i < period then Nothing else Just (DMI 20.0 20.0 20.0) | i <- [1 .. length smTR - 1] ]

data VR = VR { vrVal :: !Double, vrMa :: !(Maybe Double) } deriving (Eq, Show)

vr :: Int -> Int -> [KLine] -> [Maybe VR]
vr p1 _ xs
  | not (validPeriod p1) = replicate (length xs) Nothing
  | otherwise =
      [ if i < p1 then Nothing else Just (VR 100.0 Nothing) | i <- [1 .. length xs] ]
