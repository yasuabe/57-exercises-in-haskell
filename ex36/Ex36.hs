{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Ex36 where

import Common.Math (roundTo)
import Common.System (Converter)
import Data.Maybe (isNothing)
import Data.Semigroup (Max (..), Min (..), Sum (..))
import Data.String.Interpolate (i)
import Data.Text as T (Text, intercalate, pack, strip, toLower, unpack)
import qualified Streamly.Data.Fold as F
import Streamly.Data.Stream as S (Stream, repeatM, fromEffect, fold, fromList)
import Text.Read (readMaybe)

type Stats = (Double, Int, Int, Double) -- (average, min, max, stdDev)
type InputReader m = Converter (Maybe Int) -> m (Maybe Int)

readNumber :: Monad m => InputReader m -> Stream m (Maybe Int)
readNumber = repeatM . ($ converter)
  where
    converter t =
      if T.toLower (T.strip t) == "done"
        then Just Nothing
        else Just <$> (readMaybe (unpack t) :: Maybe Int)

collectUntilNothing :: Monad m => Stream m (Maybe Int) -> Stream m [Int]
collectUntilNothing = fromEffect
                    . S.fold (F.takeEndBy_ isNothing (F.catMaybes F.toList))

calcStats :: [Int] -> ([Int], Maybe Stats)
calcStats [] = ([], Nothing)
calcStats ts = (ts, Just (avg, minVal, maxVal, stdDev))
  where
    toSumMinMax y = (Sum 1, Sum y, Min y, Max y)
    (Sum c, Sum s, Min minVal, Max maxVal) = foldMap toSumMinMax ts
    avg = fromIntegral s / fromIntegral c
    Sum devSum = foldMap (\t -> Sum $ (fromIntegral t - avg) ^ 2) ts
    stdDev = sqrt (devSum / fromIntegral c)

makeOutput :: Monad m => ([Int], Maybe Stats) -> Stream m Text
makeOutput = S.fromList . toLines
  where
    toLines :: ([Int], Maybe Stats) -> [Text]
    toLines (inputs, Nothing) = ["No numbers were entered."]
    toLines (inputs, Just (avg, min', max', stdDev)) = 
      [ [i|The numbers are: #{numbers}|]
      , [i|The average is : #{round avg :: Int}|]
      , [i|The minimum is : #{min'}|]
      , [i|The maximum is : #{max'}|]
      , [i|The standard deviation is : #{roundTo stdDev 2}|]
      ]
      where
        numbers = T.intercalate ", " $ fmap (pack . show) inputs