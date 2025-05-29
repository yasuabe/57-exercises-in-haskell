module MainTest where

import Ex11 (exchange)
import Test.HUnit

testExchange :: Test
testExchange =
  TestCase $
    assertEqual
      "test example output from the book"
      11139
      (exchange 8100 137.51 100)

main :: IO ()
main =
  runTestTTAndExit $
    TestList [testExchange]