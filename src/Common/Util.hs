module Common.Util (promptInput, randomPositive, readWithPrompt, readIntegral, readDouble, divRatio, shuffle) where

import Control.Monad.Trans.Except ( throwE, ExceptT )
import Control.Monad.IO.Class (liftIO, MonadIO(..))
import System.IO (hFlush, stdout)
import System.Random ( getStdGen, uniformR, randomRIO )
import Text.Read (readMaybe)

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

readIntegral :: (Integral a, Read a) => String -> ExceptT String IO a
readIntegral prompt = readWithPrompt prompt "Invalid number"

readDouble :: String -> ExceptT String IO Double
readDouble prompt = readWithPrompt prompt "Invalid number"

divRatio :: (Fractional c, Real a, Real b) => a -> b -> c
divRatio a b = realToFrac a / realToFrac b

randomPositive :: (MonadIO m) => Int -> m Int
randomPositive upperBound = fst . uniformR (1, upperBound) <$> getStdGen

shuffle :: (MonadIO m) =>[a] -> m [a]
shuffle xs = go xs []
  where
    go [] acc = return acc
    go ys acc = do
      i <- randomRIO (0, length ys - 1)
      case splitAt i ys of
        (_,    []     ) -> error "Unreachable: This case should never occur."
        (left, x:right) -> go (left ++ right) (x : acc)
