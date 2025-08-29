-- Ex12: Computing Simple Interest
--
-- - Prompt for principal, interest rate (as %), and years.
-- - Compute simple interest: A = P × (1 + r × t).
-- - Convert percent rate by dividing by 100.
-- - Round up to the nearest cent.
-- - Format the output as currency.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Ex12 (calcSimpleInterest, makeOutput) where

import Data.Text (Text)
import Data.String.Interpolate (i)

calcSimpleInterest :: (Double, Double, Int) -> Double
calcSimpleInterest (principal, rate, years) =
  let r = rate / 100
      amount = principal * (1 + r * fromIntegral years)
  in fromIntegral (ceiling (amount * 100) :: Int) / 100

makeOutput :: ((Double, Double, Int), Double) -> Text
makeOutput ((_, rate, years), amount) =
  [i|After #{years} years at #{rate}%, the investment will be worth $#{amount}.|]