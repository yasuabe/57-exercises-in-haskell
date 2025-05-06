{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Common.System2 (putTextLn, readLine) where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Data.String.Interpolate (i)
import Data.Text as T (Text, pack, unpack)
import System.Console.Haskeline (InputT, getInputLine, outputStrLn)

putTextLn :: MonadIO m => Text -> InputT m ()
putTextLn = outputStrLn . unpack

readLine :: (MonadIO m, MonadMask m) => String -> InputT m Text
readLine prompt = do
  minput <- getInputLine [i|\ESC[1;32m\STX#{prompt} \ESC[0m\STX|]
  return $ maybe "" pack minput
