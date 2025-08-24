{-# LANGUAGE OverloadedStrings #-}

module Spec where

import Ex36
import Test.HUnit
import Data.Function ((&))
import Data.Maybe (isNothing, fromMaybe)
import Data.String.Interpolate (i)
import Data.Text as T (Text, intercalate, pack, strip, toLower, unpack)
import qualified Streamly.Data.Fold as F
import Streamly.Data.Stream as S (fold, mapM, fromList, toList)
import Streamly.Internal.Data.Stream
import Text.Read (readMaybe)
import Data.Functor.Identity (Identity, runIdentity)
import Control.Monad (join)
import Control.Arrow ((>>>))
import Data.Tuple.Extra (uncurry3)

testReadNumber :: Test
testReadNumber = TestList $ uncurry3 makeTestCase <$>
  [ ("should parse valid number"                   , "10"  , Just 10)
  , ("should return Nothing when 'done' is entered", "done", Nothing)
  ]
  where
    makeTestCase :: String -> Text -> Maybe Int -> Test
    makeTestCase label input expected =
      TestCase $ assertEqual label expected (runReadNumber input)

    runReadNumber = fromText
                  >>> readNumber
                  >>> S.fold F.one
                  >>> runIdentity
                  >>> join

    fromText input = ($ input) >>> join >>> pure

testCollectUntilNothing :: Test
testCollectUntilNothing = TestList $ uncurry3 makeTestCase <$>
  [ ("should collect values until Nothing is encountered"    , [Just 10, Just 20, Nothing], [10, 20])
  , ("should return empty list when first element is Nothing", [Nothing]                  , [])
  ]
  where
    makeTestCase :: String -> [Maybe Int] -> [Int] -> Test
    makeTestCase label input expected =
      TestCase $ assertEqual label expected (collection input)

    collection :: [Maybe Int] -> [Int]
    collection input = S.fromList input
                     & collectUntilNothing
                     & S.fold F.one
                     & runIdentity
                     & fromMaybe []

testCalcStats :: Test
testCalcStats = TestList $ uncurry3 makeTestCase <$>
  [ ("should calculate stats for valid input", [100, 200, 1000, 300], Just (400.0, 100, 1000, sqrt 125000))
  , ("should return Nothing for empty input" , []                   , Nothing)
  ]
  where
    makeTestCase :: String -> [Int] -> Maybe (Double, Int, Int, Double) -> Test
    makeTestCase label input expected =
      TestCase $ assertEqual label (input, expected) (calcStats input)

testMakeOutput :: Test
testMakeOutput = TestList $ uncurry3 makeTestCase <$>
  [ ( "should format output with statistics when data is available"
    , ([1], Just (400.0, 100, 1000, sqrt 125000))
    , [ "The numbers are: 1"
      , "The average is : 400"
      , "The minimum is : 100"
      , "The maximum is : 1000"
      , "The standard deviation is : 353.55"
      ]
    )
  , ( "should show appropriate message when no data is available"
    , ([1], Nothing)
    , ["No numbers were entered."]
    )
  ]
  where
    makeTestCase :: String -> ([Int], Maybe (Double, Int, Int, Double)) -> [Text] -> Test
    makeTestCase label input expected =
      TestCase $ assertEqual label expected $ runIdentity $ S.fold F.toList $ makeOutput input

main :: IO ()
main = runTestTTAndExit
     $ TestList
      [ testReadNumber
      , testCollectUntilNothing
      , testCalcStats
      , testMakeOutput
      ]