-- Exercise 11: Currency Conversion
--
-- - Prompt for:
--   - Euro amount
--   - Exchange rate
-- - Convert euros to U.S. dollars using:
--   - `amount_to = (amount_from × rate_from) / rate_to`
-- - Round up to the next cent
-- - Print result in a single output statement.

module Ex11 (exchange) where

exchange :: Double -> Double -> Double -> Double
exchange amount_from rate_from rate_to =
  let amount_to = (amount_from * rate_from) / rate_to
  in fromIntegral (ceiling amount_to :: Int) :: Double
