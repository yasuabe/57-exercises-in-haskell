-- # Ex16: Legal Driving Age
--
-- - Prompt the user for their age.
-- - Compare it to the legal driving age (16).
-- - Output a single message:
--   - If 16 or older → “You are old enough to legally drive.”
--   - If under 16 → “You are not old enough to legally drive.”
-- - Use if/else with a single print statement.
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Ex16 where

import Control.Monad.Free
import Data.String.Interpolate (i)
import Data.Text (Text, unpack)
import Text.Read (readMaybe)

data ConsoleF next
  = ReadLine  Text (Text -> next)
  | WriteLine Text next
  deriving Functor

type Console = Free ConsoleF

readLine :: Text -> Console Text
readLine prompt = liftF (ReadLine prompt id)

writeLine :: Text -> Console ()
writeLine str = liftF (WriteLine str ())

readAge :: Console Int
readAge = do
  input <- readLine "What is your age? "
  case readMaybe $ unpack input of
    Just v | v >= 0 -> return v
    _               -> writeLine "Invalid age." >> readAge

program :: Console ()
program = do
  age <- readAge
  let notWord = if age >= 16 then "" else "not " :: Text
  writeLine [i|You are #{notWord}old enough to legally drive.|]
