{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Test where
  
import Control.Monad.Free
import Data.Text as T (Text, isInfixOf)
import Data.String.Interpolate (i)
import Debug.Trace (trace)
import Test.HUnit

import Ex19

runMock :: [Text] -> Console a -> ([Text], a)
runMock _ (Pure a) = ([], a)
runMock inputs (Free (WriteLine str next)) =
  let (log, a) = runMock inputs next
  in (str : log, a)
runMock (i:is) (Free (ReadLine str f)) =
  let (log, a) = runMock is (f i)
  in (str : log, a)
runMock [] (Free (ReadLine str f)) = undefined

testBMICalculation :: Text -> Text -> Text -> Text -> Test
testBMICalculation height weight bmi message = TestCase $ do
  let [_, _, result] = fst $ runMock [height, weight] program
  assertBool [i|if height=#{height}, weight=#{weight} then #{message}|] $ T.isInfixOf message result

testInvalidInputHandling :: Test
testInvalidInputHandling = TestCase $ do
  let [_, err1, _, _, err2, _, _] = fst $ runMock ["wrong", "66.5", "bad", "116"] program
  assertEqual "Invalid height." "Invalid height." err1
  assertEqual "Invalid weight." "Invalid weight." err2

main :: IO ()
main = runTestTTAndExit $ TestList
     [ testBMICalculation "66.5" "116.0" "18.4" "underweight"
     , testBMICalculation "66.5" "116.1" "18.5" "ideal"
     , testBMICalculation "66.5" "157.5" "25.0" "ideal"
     , testBMICalculation "66.5" "157.6" "25.1" "overweight"
     , testInvalidInputHandling
     ]
