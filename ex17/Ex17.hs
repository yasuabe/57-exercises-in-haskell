-- # Ex17: Blood Alcohol Calculator
--
-- - Prompt for weight, gender, alcohol amount, and time since last drink.
-- - Compute BAC using a given formula.
-- - Report whether it's legal to drive (BAC ≥ 0.08 means illegal).
-- - Constraint: Validate that inputs are numeric.

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Ex17 where

import Common.Math (roundTo)
import Control.Monad.Free
import Data.String.Interpolate (i)
import Data.Text as T (Text, unpack, toUpper)
import Text.Read (readMaybe)
import Fmt

data Gender = Male | Female
  deriving (Eq, Show, Read, Enum, Bounded)

data ConsoleF next
  = ReadLine  Text (Text -> next)
  | WriteLine Text next
  deriving Functor

type Console = Free ConsoleF

readLine :: Text -> Console Text
readLine prompt = liftF (ReadLine prompt id)

writeLine :: Text -> Console ()
writeLine str = liftF (WriteLine str ())

readDouble :: Text -> Text -> Console Double 
readDouble prompt errMsg = do
  input <- readLine prompt
  case readMaybe $ unpack input of
    Just v | v >= 0 -> return v
    _               -> writeLine errMsg >> readDouble prompt errMsg

readGender :: Console Gender
readGender = do
  gender  <- readLine "What is your gender? "
  case T.toUpper gender of
    "M" -> return Male
    "F" -> return Female
    _   -> writeLine "Invalid gender." >> readGender

readWeight :: Console Double
readWeight = readDouble "What is your weight? " "Invalid weight."

readAlcohol :: Console Double
readAlcohol = readDouble "How much alcohol have you consumed? " "Invalid alcohol amount."

readHours :: Console Double 
readHours = readDouble "How many hours has it been since your last drink? " "Invalid hours."

calculateBAC :: Double -> Gender -> Double -> Double -> Double
calculateBAC w gender a h =
  (a * 5.14) / (w * r) - (0.015 * h)
  where
    r = case gender of
      Male   -> 0.73
      Female -> 0.66

program :: Console ()
program = do
  weight  <- readWeight
  gender  <- readGender
  alcohol <- readAlcohol
  hours   <- readHours

  let bac     = calculateBAC weight gender alcohol hours
  let negator = if bac < 0.08 then "" else "not " :: Text

  writeLine [i|Your BAC is #{fixedF 3 bac}|]
  writeLine [i|It is #{negator}legal for you to drive.|]
