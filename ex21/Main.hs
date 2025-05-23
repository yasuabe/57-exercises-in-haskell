--  Exercise 21: Numbers to Names
--
-- ・Prompt the user to enter a number from 1 to 12.
-- ・Display the corresponding month name (e.g., 1 → January).
-- ・If the number is outside this range, show an error message.
-- ・Use a switch or case statement.
-- ・Use a single output statement.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad (mfilter)
import Data.Functor ((<&>))
import Data.String.Interpolate (i)
import Data.Text (Text)
import System.Console.Haskeline (InputT)

import Common.App (runProgram)
import Common.Math (between)
import Common.System (convertText, repeatUntilValid, putTextLn)

type AppType = InputT IO

readMonthNumber :: AppType Int
readMonthNumber =
  repeatUntilValid
    (mfilter (between 1 12) . convertText)
    "Please enter the number of the month: "
    "Invalid input. Enter a number between 1 and 12."

program :: AppType ()
program =
  readMonthNumber
  <&> \case
    1  -> "January"
    2  -> "February"
    3  -> "March"
    4  -> "April"
    5  -> "May"
    6  -> "June"
    7  -> "July"
    8  -> "August"
    9  -> "September"
    10 -> "October"
    11 -> "November"
    12 -> "December"
    _  -> error "This should never happen"
  >>= \name -> putTextLn [i|The name of the month is #{name::Text}.|]

main :: IO ()
main = runProgram program
