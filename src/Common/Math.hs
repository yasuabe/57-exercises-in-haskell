module Common.Math where

roundTo :: Double -> Int -> Double
roundTo x n = fromIntegral (round (x * 10^n) :: Integer) / 10^n