{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Common.System (Converter, YesNo(..), askYesNo, putText, putTextLn, repeatUntilValid, readLine, toNonNegativeInt, toNonNega2Decimals, toPositiveSmallInt, convertText, readValid) where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Data.Function ((&))
import Data.String.Interpolate (i)
import Data.Text as T (Text, pack, strip, toLower, unpack)
import System.Console.Haskeline (InputT, getInputLine, outputStr, outputStrLn)
import Text.Read (readMaybe)
import Text.Regex.TDFA ((=~))

type Converter a = Text -> Maybe a

putText :: MonadIO m => Text -> InputT m ()
putText = outputStr . unpack

putTextLn :: MonadIO m => Text -> InputT m ()
putTextLn = outputStrLn . unpack

readLine :: (MonadIO m, MonadMask m) => Text -> InputT m Text
readLine prompt = do
  minput <- getInputLine [i|\ESC[1;32m\STX#{prompt} \ESC[0m\STX|]
  return $ maybe "" pack minput

repeatUntilValid :: Converter a -> Text -> Text -> InputT IO a
repeatUntilValid converter prompt errorMessage = do
  input <- readLine prompt
  case converter input of
    Just value -> return value
    Nothing    -> do
      putTextLn errorMessage
      repeatUntilValid converter prompt errorMessage

convertText :: (Read a) => Converter a 
convertText input =
  case input & T.strip & unpack & reads of
    [(x, "")] -> Just x
    _         -> Nothing

readValid :: (Read a) => Text -> Text -> InputT IO a
readValid prompt errorMessage = do
  repeatUntilValid convertText prompt errorMessage

matchConvert :: (Read a) => String -> Converter a
matchConvert pattern t
  | t =~ pattern = readMaybe (unpack t)
  | otherwise    = Nothing

toNonNegativeInt :: Converter Int
toNonNegativeInt = matchConvert "^(0|[1-9][0-9]*)$"

toPositiveSmallInt :: Converter Int
toPositiveSmallInt = matchConvert "^([1-9][0-9]{0,2})$"

toNonNega2Decimals :: Converter Double
toNonNega2Decimals = matchConvert "^(0|[1-9][0-9]*)(\\.[0-9]{1,2})?$"

data YesNo = Yes | No

askYesNo :: Text -> InputT IO YesNo
askYesNo prompt =
  repeatUntilValid convertToYesNo
                   prompt
                   "Enter yes/no or y/n (not case sensitive)."
  where
    convertToYesNo :: Text -> Maybe YesNo
    convertToYesNo t
      | t' == "yes" || t' == "y" = Just Yes
      | t' == "no"  || t' == "n" = Just No
      | otherwise                = Nothing
      where
        t' = t & strip & toLower
