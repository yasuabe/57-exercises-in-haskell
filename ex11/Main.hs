-- Exercise 11: Currency Conversion
--
-- - Prompt for:
--   - Euro amount
--   - Exchange rate
-- - Convert euros to U.S. dollars using:
--   - `amount_to = (amount_from × rate_from) / rate_to`
-- - Round up to the next cent
-- - Print result in a single output statement.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Common.App (runProgram)
import Common.System (putTextLn, repeatUntilValid, toNonNega2Decimals)
import Data.String.Interpolate (__i)
import Ex11 (exchange)
import System.Console.Haskeline (InputT)

type AppType = InputT IO

program :: AppType ()
program = do
  euro <- read2Decimals "How many euros are you exchanging? " "Invalid euro"
  rate <- read2Decimals "What is the exchange rate? "         "Invalid rate"
  let usCent = exchange (euro * 100) rate 100

  putTextLn
    [__i|#{euro} euros at an exchange rate of #{rate} is
         #{usCent / 100.0} U.S. dollars.|]
  where
    read2Decimals = repeatUntilValid toNonNega2Decimals

main :: IO ()
main = runProgram program
