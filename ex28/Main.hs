{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- # Ex28: Adding Numbers
-- ・ Prompt the user to enter five numbers.
-- ・ Use a counted loop to handle repeated prompting.
-- ・ Compute the total of the entered numbers.
-- ・ Display the total at the end.

module Main where

import Control.Monad  (replicateM)
import Data.String.Interpolate (i)
import System.Console.Haskeline (InputT)

import Common.App (runProgram)
import Common.System (putTextLn, repeatUntilValid, readDouble)

readFiveNumbers :: InputT IO [Double]
readFiveNumbers =
    replicateM 5 readNumber
  where
    readNumber = repeatUntilValid readDouble "Enter a number: " "Invalid number"

program :: InputT IO ()
program = do
  numbers <- readFiveNumbers
  let total = sum numbers
  putTextLn [i|The total is #{total}.|]

main :: IO ()
main = runProgram program
