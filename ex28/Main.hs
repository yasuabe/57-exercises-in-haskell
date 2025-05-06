{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- # Ex28: Adding Numbers
-- ・ Prompt the user to enter five numbers.
-- ・ Use a counted loop to handle repeated prompting.
-- ・ Compute the total of the entered numbers.
-- ・ Display the total at the end.

module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad  (replicateM)
import Data.Function ((&))
import Data.String.Interpolate (i)
import Data.Text as T (Text, pack, strip, unpack)
import System.IO (hFlush, stdout)

type AppType a = ExceptT String IO a
type Converter a = Text -> Maybe a

putText :: Text -> IO ()
putText text = putStr (unpack text) >> hFlush stdout

putTextLn :: Text -> IO ()
putTextLn text = putStrLn (unpack text) >> hFlush stdout

promptInput :: Text -> IO Text
promptInput promptText = do
  putText promptText
  T.strip . pack <$> getLine

repeatUntilValid :: Converter a -> Text -> Text -> IO a
repeatUntilValid converter prompt errorMessage = do
  input <- promptInput prompt
  case converter input of
    Just value -> return value
    Nothing    -> do
      putTextLn errorMessage
      repeatUntilValid converter prompt errorMessage

maybeIf :: Bool -> a -> Maybe a
maybeIf b a = if b then Just a else Nothing

readDouble :: Text -> Maybe Double
readDouble input =
  case input & T.strip & unpack & reads of
    [(x, "")] -> Just x
    _         -> Nothing

readNumber :: AppType Double
readNumber = liftIO
           $ repeatUntilValid readDouble "Enter a number: " "Invalid number"

readFiveNumbers :: AppType [Double]
readFiveNumbers = replicateM 5 readNumber

program :: AppType ()
program = do
  numbers <- readFiveNumbers
  let total = sum numbers
  liftIO $ putTextLn [i|The total is #{total}.|]

runProgram :: AppType () -> IO ()
runProgram app = runExceptT app >>= either (putStrLn . ("Error: " ++)) (const (return ()))

main :: IO ()
main = runProgram program
