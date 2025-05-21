{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- Ex13: Determining Compound Interest
-- 
-- ・Prompt the user for principal amount, interest rate (as a percentage), number of years, and compounding frequency per year.
-- ・Convert the interest rate by dividing it by 100.
-- ・Use the compound interest formula to compute the final amount.
-- ・Round up fractions of a cent to the next penny.
-- ・Format the output as money.

module Main where

import Data.String.Interpolate (i)
import Data.Text (Text, unlines)

import System.Console.Haskeline (InputT)

import Common.App (runProgram)
import Common.System (putTextLn, repeatUntilValid,
                      toNonNega2Decimals, toPositiveSmallInt, toNonNegativeInt)
import Ex13 (calcCompoundRate)

data InputParam = InputParam
  { principal :: Int
  , rate      :: Double
  , years     :: Int
  , frequency :: Int
  } deriving (Show)

getPrincipalAmount :: InputT IO Int
getPrincipalAmount = repeatUntilValid
                       toNonNegativeInt
                       "What is the principal amount: "
                       "Enter a non-negative integer."
getAnnualInterestRate :: InputT IO Double
getAnnualInterestRate = repeatUntilValid
                       toNonNega2Decimals
                       "What is the rate: "
                       "Enter a positive decimal rounded to two decimal places"
getNumberOfYears :: InputT IO Int
getNumberOfYears = repeatUntilValid
                       toPositiveSmallInt
                       "What is the number of years: "
                       "Enter 1 ~ 999."
getFrequency :: InputT IO Int
getFrequency = repeatUntilValid
                       toPositiveSmallInt
                       "What is the number of times the interest is compounded per year? "
                       "Enter 1 ~ 999."

getInputs :: InputT IO InputParam
getInputs = InputParam
          <$> getPrincipalAmount
          <*> getAnnualInterestRate
          <*> getNumberOfYears
          <*> getFrequency

displayResult :: InputParam -> Double -> InputT IO ()
displayResult input componded =
  let InputParam p r y f = input
      compounded' = fromIntegral (ceiling (componded * 100) :: Integer) / 100 :: Double
      formatted = [ [i|$#{p} invested at #{r}% for #{y} years|]
                  , [i|compounded #{f} times per year is $#{compounded'}.|]
                  ] :: [Text]
  in putTextLn $ Data.Text.unlines formatted

program :: InputT IO ()
program = do
  input@(InputParam p r y f) <- getInputs
  let compounded = calcCompoundRate p r y f
  displayResult input compounded

main :: IO ()
main = runProgram program
