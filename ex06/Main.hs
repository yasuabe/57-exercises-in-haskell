{-# LANGUAGE QuasiQuotes #-}

-- Ex6: Retirement Calculator
-- ・Prompt the user to enter their current age and desired retirement age.
-- ・Convert input to numeric values before performing calculations.
-- ・Determine how many years are left until retirement.
-- ・Get the current year from the system, not hard-coded.
-- ・Calculate and display the retirement year and remaining years.

module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT, ExceptT)
import Data.String.Interpolate (i)
import Data.Time (getCurrentTime, utctDay, toGregorian, Year)

import Common.Util (readIntegral)

type AppType a = ExceptT String IO a

getYear :: AppType Year
getYear = liftIO $ do
  utcTime <- getCurrentTime
  let (year, _, _) = toGregorian (utctDay utcTime)
  return year

program :: AppType ()
program = do
  currentAge    <- readIntegral "What is your current age? "
  retirementAge <- readIntegral "At what age would you like to retire? "
  currentYear   <- getYear

  let yearsLeft      = retirementAge - currentAge
      retirementYear = currentYear + yearsLeft 

  liftIO $ putStrLn [i|You have #{yearsLeft} years left until you can retire.|]
  liftIO $ putStrLn [i|It's #{currentYear}, so you can retire in #{retirementYear}.|]

main :: IO ()
main = runExceptT program >>= either (putStrLn . ("Error: " ++)) (const (return ()))
