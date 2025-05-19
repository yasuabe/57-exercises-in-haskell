{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

-- ・Ex38: Filtering Values
-- 
-- ・Prompt the user to enter a space-separated list of numbers.
-- ・Convert the input string into an array.
-- ・Use a function `filterEvenNumbers (old_array)` to return a new array with only even numbers.
-- ・Do not use built-in filter or similar features.
-- ・Print the filtered even numbers.

module Main where

import Data.String.Interpolate (i)
import Data.Text as T (Text, words, unwords, strip, unpack, null)
import Streamly.Data.Stream as S (fromEffect, concatMap, mapM, fold, mapM)
import Streamly.Internal.Data.Stream
import qualified Streamly.Data.Fold as F
import System.Console.Haskeline (InputT)
import Text.Read (readMaybe)

import Common.System ( readLine, putTextLn )
import Common.App (runProgram)
import Data.Function ((&))

filterEvenNumbers:: Monad m => Stream m Text -> Stream m Text
filterEvenNumbers (Stream step state) = Stream step' state
  where
    step' gst st = step gst st >>= \case
      Yield x s -> return $ if evenText x then Yield x s else Skip s
      Skip s    -> return $ Skip s
      Stop      -> return Stop
    evenText t = maybe False even (readMaybe (unpack t) :: Maybe Int)

readNumbersLine :: Stream (InputT IO) Text
readNumbersLine = fromEffect $ readLine "Enter a space-separated list of numbers: "

lineToWords :: Monad m => Stream m Text -> Stream m Text
lineToWords = S.concatMap (fromList . T.words . T.strip)

wordsToLine :: Monad m => Stream m Text -> Stream m Text
wordsToLine = S.fromEffect . fmap T.unwords . S.fold F.toList

program :: InputT IO ()
program = readNumbersLine
        & lineToWords
        & filterEvenNumbers
        & wordsToLine
        & printResult
        & S.fold F.drain
  where
    printResult = S.mapM (putTextLn . outputText)

    outputText :: Text -> Text
    outputText t = if T.null t
                   then "No even numbers found."
                   else [i|The even numbers are #{t}|]

main :: IO ()
main = runProgram program