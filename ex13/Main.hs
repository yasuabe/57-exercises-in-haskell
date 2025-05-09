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
import Data.Text (Text, unpack, unlines)
import Text.Read (readMaybe)
import Text.Regex.TDFA ((=~))

import System.Console.Haskeline (InputT, Completion (display), outputStr)

import Common.App (runProgram)
import Common.System (Converter, putTextLn, repeatUntilValid)
import Ex13 (calcCompoundRate)

data InputParam = InputParam
  { principal :: Int
  , rate      :: Double
  , years     :: Int
  , frequency :: Int
  } deriving (Show)

matchConvert :: (Read a) => String -> Converter a
matchConvert pattern t
  | t =~ pattern = readMaybe (unpack t)
  | otherwise    = Nothing

toNonNegativeInt :: Converter Int
toNonNegativeInt = matchConvert "^(0|[1-9][0-9]*)$"

toPositiveSmallInt :: Converter Int
toPositiveSmallInt = matchConvert "^([1-9][0-9]{0,2})$"

toNonNegativeRate :: Converter Double
toNonNegativeRate = matchConvert "^(0|[1-9][0-9]*)(\\.[0-9]{1,2})?$"

getPrincipalAmount :: InputT IO Int
getPrincipalAmount = repeatUntilValid
                       toNonNegativeInt
                       "What is the principal amount: "
                       "Enter a non-negative integer."
getAnnualInterestRate :: InputT IO Double
getAnnualInterestRate = repeatUntilValid
                       toNonNegativeRate
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
      compounded' = fromIntegral (ceiling (componded * 100)) / 100 :: Double
      formatted = [ [i|$#{p} invested at #{r}% for #{y} years|]
                  , [i|compounded #{f} times per year is $#{compounded'}.|]
                  ] :: [Text]
  in putTextLn $ Data.Text.unlines formatted

program :: InputT IO ()
program = do
  input@(InputParam principal rate year frequency) <- getInputs
  let compounded = calcCompoundRate principal rate year frequency
  displayResult input compounded

main :: IO ()
main = runProgram program
