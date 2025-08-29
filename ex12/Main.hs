-- Ex12: Computing Simple Interest
--
-- - Prompt for principal, interest rate (as %), and years.
-- - Compute simple interest: A = P × (1 + r × t).
-- - Convert percent rate by dividing by 100.
-- - Round up to the nearest cent.
-- - Format the output as currency.
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common.App (runProgram)
import Common.System (putTextLn, repeatUntilValid, toNonNega2Decimals, toNonNegativeInt)
import Common.Function (fproduct)
import Ex12 (calcSimpleInterest, makeOutput)
import System.Console.Haskeline (InputT)
import Data.Function ((&))
import qualified Streamly.Data.Fold as F
import Streamly.Data.Stream as S (fold, mapM, fromEffect)

type AppType = InputT IO

program :: AppType ()
program = fromEffect readInput
        & fproduct calcSimpleInterest
        & fmap makeOutput
        & S.mapM putTextLn
        & S.fold F.drain
  where
    readPrincipal = repeatUntilValid toNonNega2Decimals "Enter principal amount: "     "Invalid principal amount."
    readRate      = repeatUntilValid toNonNega2Decimals "Enter interest rate (as %): " "Invalid interest rate."
    readYear      = repeatUntilValid toNonNegativeInt   "Enter number of years: "      "Invalid number of years."

    readInput :: AppType (Double, Double, Int)
    readInput = (,,) <$> readPrincipal <*> readRate <*> readYear

main :: IO ()
main = runProgram program
