{-# LANGUAGE OverloadedStrings #-}

module Test where
  
import Control.Monad.Free
import Data.Text as T (Text, isInfixOf)
import Test.HUnit

import Ex16

runMock :: [Text] -> Console a -> ([Text], a)
runMock _ (Pure a) = ([], a)
runMock inputs (Free (WriteLine str next)) =
  let (log, a) = runMock inputs next
  in (str : log, a)
runMock (i:is) (Free (ReadLine str f)) =
  let (log, a) = runMock is (f i)
  in (str : log, a)

testLegalAge :: Test
testLegalAge = TestCase $ do
  let [_, result] = fst $ runMock ["16"] program
  assertBool "16 is legal" $ T.isInfixOf "are old enough" result

testIllegalAge :: Test
testIllegalAge = TestCase $ do
  let [_, result] = fst $ runMock ["15"] program
  assertBool "15 is illegal" $ T.isInfixOf "are not old enough" result

testInvalidInput :: Test
testInvalidInput = TestCase $ do
  let [_, errMsg, _, _] = fst $ runMock ["x", "16"] program
  assertBool "x is invalid input" $ T.isInfixOf "Invalid" errMsg 

main :: IO ()
main = runTestTTAndExit $ TestList
     [ testLegalAge
     , testIllegalAge
     , testInvalidInput
     ]
