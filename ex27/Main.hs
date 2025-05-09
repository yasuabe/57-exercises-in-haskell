{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- Ex27: Validating Inputs
--
-- ・ Prompt the user for first name, last name, ZIP code, and employee ID.
-- ・ Validate each input:
--   ・First and last names must not be empty and must be at least two characters long.
--   ・ZIP code must be numeric.
--   ・Employee ID must match the format AA-1234.
-- ・ Create a separate function for each validation.
-- ・ Create a validateInput function to coordinate all validations.
-- ・ Use a single output statement to display all error messages or success.

module Main where

import Data.String.Interpolate (i)
import Data.Validation ( Validation(Failure, Success) )
import System.Console.Haskeline (InputT)

import Common.App (runProgram)
import Common.System (readLine, putTextLn)
import Validation (Input, ValidationResult, validateInput)

getInput :: InputT IO Input
getInput = do
  firstName <- readLine "Enter the first name: "
  lastName  <- readLine "Enter the last name: "
  zipCode   <- readLine "Enter the zIP code: "
  empID     <- readLine "Enter an employee ID: "
  return (firstName, lastName, zipCode, empID)

displayResult :: ValidationResult Input -> InputT IO ()
displayResult (Success _)      = putTextLn [i|There were no errors found.|]
displayResult (Failure errors) = mapM_ putTextLn errors

main :: IO ()
main = runProgram
     $ getInput
     >>= displayResult . validateInput