{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- # Ex28: Adding Numbers
-- ・ Prompt the user to enter five numbers.
-- ・ Use a counted loop to handle repeated prompting.
-- ・ Compute the total of the entered numbers.
-- ・ Display the total at the end.

module Main where

import Control.Monad  (replicateM)
import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Data.String.Interpolate (i)

import Common.App (AppType, run)
import Common.System (putTextLn, repeatUntilValid, readDouble)

readFiveNumbers :: AppType [Double]
readFiveNumbers =
    replicateM 5 readNumber & liftIO
  where
    readNumber = repeatUntilValid readDouble "Enter a number: " "Invalid number"

program :: AppType ()
program = do
  numbers <- readFiveNumbers
  let total = sum numbers
  putTextLn [i|The total is #{total}.|] & liftIO

main :: IO ()
main = run program
