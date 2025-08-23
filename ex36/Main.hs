{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- # Ex36: Computing Statistics
--
-- - Prompt the user to enter numbers representing response times until “done” is entered.
-- - Store the numeric inputs in an array, excluding “done”.
-- - Compute and display the average, minimum, maximum, and standard deviation.
-- - Use loops and arrays for input and calculations.
-- - Keep input, processing, and output logic separate.

module Main where

import Common.App (runProgram)
import Common.Math (roundTo)
import Common.System (putTextLn, repeatUntilValid)
import Data.Function ((&))
import Data.Maybe (isNothing)
import Data.Semigroup (Max (..), Min (..), Sum (..))
import Data.String.Interpolate (i)
import Data.Text as T (Text, intercalate, pack, strip, toLower, unpack)
import qualified Streamly.Data.Fold as F
import Streamly.Data.Stream as S (fold, mapM)
import Streamly.Internal.Data.Stream
import System.Console.Haskeline (InputT)
import Text.Read (readMaybe)

type AppType = InputT IO

calcStats :: [Int] -> (Double, Int, Int, Double)
calcStats ts =
  ( avg
  , n
  , x
  , sqrt (devSum / fromIntegral c)
  )
  where
    toSumMinMax y = (Sum (1 :: Int), Sum y, Min y, Max y)
    (Sum c, Sum s, Min n, Max x) = foldMap toSumMinMax ts
    avg = fromIntegral s / fromIntegral c
    Sum devSum = foldMap (\t -> Sum $ (fromIntegral t - avg) ^ (2::Int)) ts

printResult :: [Int] -> AppType ()
printResult ts = do
  putTextLn [i|The numbers are: #{numbers}|]
  putTextLn [i|The average is : #{roundTo avg 0}|]
  putTextLn [i|The minimum is : #{min'}|]
  putTextLn [i|The maximum is : #{max'}|]
  putTextLn [i|The standard deviation is : #{roundTo stdDev 2}|]
  where
    numbers = T.intercalate ", " $ fmap (pack . show) ts
    (avg, min', max', stdDev) = calcStats ts

program :: AppType ()
program = readNumber
        & toIntListStream
        & S.mapM printResult
        & S.fold F.drain
  where
    readNumber :: Stream AppType (Maybe Int)
    readNumber =
      repeatM $
        repeatUntilValid
          converter
          "Enter a number: "
          "Enter a valid non-negative number"
      where
        converter :: Text -> Maybe (Maybe Int)
        converter t =
          if T.toLower (T.strip t) == "done"
            then Just Nothing
            else Just <$> (readMaybe (unpack t) :: Maybe Int)

    toIntListStream :: Stream AppType (Maybe Int) -> Stream AppType [Int]
    toIntListStream upper =
      fromEffect $ S.fold collectUntilNothing upper
      where
        collectUntilNothing = F.takeEndBy_ isNothing (F.catMaybes F.toList)

main :: IO ()
main = runProgram program
