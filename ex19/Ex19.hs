-- Ex19: BMI Calculator
--
-- - Prompt the user for their height (in inches) and weight (in pounds).
-- - Calculate BMI using:
-- - ` bmi = (weight / (height × height)) × 703 ``
-- - Display:
--   - “You are within the ideal weight range.” if BMI is 18.5–25.
--   - Otherwise, indicate if the user is underweight or overweight and advise seeing a doctor.
-- - Input must be numeric—reject non-numeric input.

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Ex19 where

import Control.Monad.Free
import Data.String.Interpolate (i)
import Data.Text (Text, unpack)
import Text.Read (readMaybe)
import Common.Math (roundTo)

data ConsoleF next
  = ReadLine  Text (Text -> next)
  | WriteLine Text next
  deriving Functor

type Console = Free ConsoleF

-- TODO: reduce duplication: (see Ex16.hs)
readLine :: Text -> Console Text
readLine prompt = liftF (ReadLine prompt id)

writeLine :: Text -> Console ()
writeLine str = liftF (WriteLine str ())

readMeasurement :: Text -> Text -> Console Double 
readMeasurement measurement unit = do
  input <- readLine [i|Enter your #{measurement} in #{unit}: |]
  case readMaybe $ unpack input of
    Just v | v >= 0 -> return v
    _               -> writeLine [i|Invalid #{measurement}.|] >> readMeasurement measurement unit

calculateBMI :: Double -> Double -> Double
calculateBMI height weight =
  flip roundTo 1 $ (weight / (height * height)) * 703

selectMessage :: Double -> Text
selectMessage bmi
  | bmi < 18.5 = "You are underweight. You should see your doctor."
  | bmi > 25   = "You are overweight. You should see your doctor."
  | otherwise  = "You are within the ideal weight range."

program :: Console ()
program = do
  height <- readMeasurement "height" "inches"
  weight <- readMeasurement "weight" "pounds"

  let bmi = calculateBMI  height weight
  writeLine $ selectMessage bmi
