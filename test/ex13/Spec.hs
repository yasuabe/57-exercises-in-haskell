module Spec where
  
import Data.Functor ((<&>))
import Data.String.Interpolate (i)
import Debug.Trace (trace)
import Test.QuickCheck
import Test.QuickCheck.Gen (choose)
import Test.QuickCheck.Property (Property)

import Ex13 (calcCompoundRate)

nearlyEqual :: Double -> Double -> Bool
nearlyEqual x y = abs (x - y) < ε
  where
    ε :: Double
    ε = 0.0001

gen :: Gen (Int, Double, Int, Int)
gen = do
  p <- choose (0, 10000 :: Int)     -- princial amount
  r <- choose (0, 200) <&> (/ 10.0) -- annual rate 
  t <- choose (1, 10 :: Int)        -- number of years
  n <- choose (1, 10 :: Int)        -- yearly compounding times
  return (p, r, t, n)

prop_calcCompoundedRate:: Property
prop_calcCompoundedRate = forAll gen $ \(p, r, t, n) ->
  let actual    = calcCompoundRate p r t n
      backwards = calcBackwords actual p r t n
  in nearlyEqual backwards 1.0
  where
    calcBackwords actual p r t n =
      let n' = fromIntegral n
          t' = fromIntegral t
          p' = fromIntegral p
      in (actual / p') ** (1 / (t' * n')) - (r / 100) / n'

main :: IO ()
main = quickCheck prop_calcCompoundedRate
