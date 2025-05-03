module Common.Util (promptInput, readWithPrompt, readDouble, divRatio) where

import System.IO (hFlush, stdout)
import Control.Monad.Trans.Except
import Text.Read (readMaybe)
import Control.Monad.IO.Class (liftIO)

promptInput :: String -> IO String
promptInput promptText = do
  putStr promptText
  hFlush stdout
  getLine


readWithPrompt :: Read a => String -> String -> ExceptT String IO a
readWithPrompt prompt msg =
  liftIO (promptInput prompt) >>= convert
  where
    convert line = case readMaybe line of
      Just x  -> return x
      Nothing -> throwE msg

readDouble :: String -> ExceptT String IO Double
readDouble prompt = readWithPrompt prompt "Invalid number"

divRatio :: (Fractional c, Real a, Real b) => a -> b -> c
divRatio a b = realToFrac a / realToFrac b
