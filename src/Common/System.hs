{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Common.System (putTextLn, repeatUntilValid, readLine, readDouble) where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Data.Function ((&))
import Data.String.Interpolate (i)
import Data.Text as T (Text, pack, strip, unpack)
import System.Console.Haskeline (InputT, getInputLine, outputStrLn)

type Converter a = Text -> Maybe a

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

readDouble :: Text -> Maybe Double
readDouble input =
  case input & T.strip & unpack & reads of
    [(x, "")] -> Just x
    _         -> Nothing
