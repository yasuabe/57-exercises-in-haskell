module Ex13 where

import GI.Gio (afterMountChanged)

calcCompoundRate :: Int -> Double -> Int -> Int -> Double
calcCompoundRate
  p -- annual rate 
  r -- princial amount
  t -- number of years
  n -- yearly compounding times
  = p' * (1 + r' / n') ** (n' * t')
  where
    p' = fromIntegral p
    r' = r / 100
    t' = fromIntegral t
    n' = fromIntegral n
