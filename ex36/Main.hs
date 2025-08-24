{-# LANGUAGE OverloadedStrings #-}

-- # Ex36: Computing Statistics
--
-- - Prompt the user to enter numbers representing response times until “done” is entered.
-- - Store the numeric inputs in an array, excluding “done”.
-- - Compute and display the average, minimum, maximum, and standard deviation.
-- - Use loops and arrays for input and calculations.
-- - Keep input, processing, and output logic separate.

module Main where

import Common.App (runProgram)
import Common.System (putTextLn, repeatUntilValid)
import Data.Function ((&))
import qualified Streamly.Data.Fold as F
import Streamly.Data.Stream as S (fold, mapM, concatMap)
import System.Console.Haskeline (InputT)
import Ex36 (readNumber, collectUntilNothing, calcStats, makeOutput)

type AppType = InputT IO

program :: AppType ()
program = readNumber reader
        & collectUntilNothing
        & fmap calcStats
        & S.concatMap makeOutput
        & S.mapM putTextLn
        & S.fold F.drain
  where
    reader converter = repeatUntilValid
          converter
          "Enter response time (or 'done' to finish): "
          "Please enter a valid response time (integer) or 'done' to finish"

main :: IO ()
main = runProgram program
