-- Ex18: Temperature Converter
--
-- ・Prompt the user to choose conversion type: Fahrenheit ↔ Celsius.
-- ・Accept both uppercase and lowercase (C, F).
-- ・Prompt for the input temperature based on the choice.
-- ・Convert using the appropriate formula.
-- ・Display the result using minimal and non-redundant output statements.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.String.Interpolate (i)
import System.Console.Haskeline (InputT)

import Common.App (runProgram)
import Common.Math (roundTo)
import Common.System (putTextLn, repeatUntilValid, readDouble)

import qualified Ex18 (ConversionType, toConversionType, convert, from, to)

type AppType a = InputT IO a

readConversionType :: AppType Ex18.ConversionType
readConversionType =
  repeatUntilValid
    Ex18.toConversionType
    "Your choice: "
    "Please enter 'C' or 'F'."

getTemperature :: Ex18.ConversionType -> AppType Double
getTemperature convType =
  repeatUntilValid
    readDouble
    [i|Please enter the temperature in #{Ex18.from convType}: |]
    "Invalid temperature"

printResult :: Ex18.ConversionType -> Double -> AppType ()
printResult convType temp =
  putTextLn [i|The temperature in #{Ex18.to convType} is #{roundTo temp 2}.|]

program :: AppType ()
program = do
  putTextLn "Press C to convert from Fahrenheit to Celsius."
  putTextLn "Press F to convert from Celsius to Fahrenheit."
  convType <- readConversionType
  tempFrom <- getTemperature convType

  let tempTo = Ex18.convert convType tempFrom

  printResult convType tempTo

main :: IO ()
main = runProgram program
