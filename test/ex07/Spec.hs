module Main where
import Ex07 ( toSquareMeter )
import Test.QuickCheck ( quickCheck )
import Data.String.Interpolate (i)
import Debug.Trace (trace)

ε :: Double
ε = 0.0001

nearlyEqual :: Double -> Double -> Bool
nearlyEqual x y = abs (x - y) < ε

prop_toSquareMeter:: Double -> Bool
prop_toSquareMeter f² =
  let m² = toSquareMeter f²
  in if f² == 0 then m² == 0 else nearlyEqual (m² / f²) k
  where
    k = 0.09290304

main :: IO ()
main = quickCheck prop_toSquareMeter
