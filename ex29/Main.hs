{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- # Ex29: Handling Bad Input
--
-- - Prompt the user for the rate of return.
-- - Keep prompting until a valid, non-zero numeric value is entered.
-- - Use the formula years = 72 / r to calculate the years to double the investment.
-- - Display the result after receiving valid input.
-- - Use a loop to handle invalid input without exiting the program.

module Main where

import Common.App (runProgram)
import Common.System (convertText, putTextLn, repeatUntilValid)
import Control.Monad (mfilter)
import Data.String.Interpolate (i)
import System.Console.Haskeline (InputT)
import Ex29 (ruleOf72)

readNaturalNumber :: InputT IO Int
readNaturalNumber =
  repeatUntilValid
    (mfilter (> 0) . convertText)
    "What is the rate of return? "
    "Sorry. That's not a valid input."

program :: InputT IO ()
program = do
  rate <- readNaturalNumber
  let years = ruleOf72 rate
  putTextLn [i|It will take #{show years} years to double your initial investment.|]

main :: IO ()
main = runProgram program
