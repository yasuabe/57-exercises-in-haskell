{-# LANGUAGE OverloadedStrings #-}

module Test where
  
import Control.Monad.Free
import Data.Text as T (Text, isInfixOf, unpack)
import Test.HUnit
import qualified Debug.Trace as Debug

import Ex17
import Text.Read.Lex (expect)

runMock :: [Text] -> Console a -> ([Text], a)
runMock _ (Pure a) = ([], a)
runMock inputs (Free (WriteLine str next)) =
  let (log, a) = runMock inputs next
  in (str : log, a)
runMock (i:is) (Free (ReadLine str f)) =
  let (log, a) = runMock is (f i)
  in (str : log, a)

testValidInput :: [Text] -> [Text] -> Test
testValidInput inputs [expect1, expect2] = TestCase $ do
  let ([_, _, _, _, output1, output2], o) = runMock inputs program
  assertEqual "line 1" expect1 output1
  assertEqual "line 2" expect2 output2

testLegalBAC :: Test
testLegalBAC = testValidInput
                 ["180", "M", "2", "2"]
                 ["Your BAC is 0.048", "It is legal for you to drive."]

testIllegalBAC :: Test
testIllegalBAC = testValidInput
                 ["150", "F", "2", "1"]
                 ["Your BAC is 0.089", "It is not legal for you to drive."]

testInvalidInput :: Test
testInvalidInput = TestCase $ do
  let ( [ _, e1, _
        , _, e2, _
        , _, e3, _
        , _, e4, _
        , _
        , _
        ]
        , o) = runMock [ "-", "180"
                       , "-", "M"
                       , "-", "2"
                       , "-", "2"
                       ] program
  assertBool "Invalid weight"  $ T.isInfixOf "Invalid weight"  e1
  assertBool "Invalid gender"  $ T.isInfixOf "Invalid gender"  e2
  assertBool "Invalid alcohol" $ T.isInfixOf "Invalid alcohol" e3
  assertBool "Invalid hours"   $ T.isInfixOf "Invalid hours"   e4

main :: IO ()
main = runTestTTAndExit $ TestList
     [ testLegalBAC
     , testIllegalBAC
     , testInvalidInput
     ]
