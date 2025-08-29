module MainTest where

import Ex12
import Test.HUnit
import Data.Tuple.Extra (uncurry3)
import Streamly.Internal.Data.SVar.Type (Rate(rateBuffer))

testCalcSimpleInterest :: Test
testCalcSimpleInterest = TestList $ uncurry3 makeTestCase <$>
  -- test name                         | principal| rate| years 
  [("test example output from the book", (    1500,  4.3,    4), 1758)
  ,("should round up at digit 2"       , (      11,  1.1,   11), 12.34)
  ]
  where
    makeTestCase testName (principal, rate, years) expected =
      TestCase $
        assertEqual
          testName
          expected
          (calcSimpleInterest (principal, rate, years))

main :: IO ()
main =
  runTestTTAndExit $
    TestList [testCalcSimpleInterest]