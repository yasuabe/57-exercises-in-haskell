{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module TokenInfoTest where

import Test.HUnit
import TokenInfoRes as TIR
import TokenInfo (TokenInfo(..), makeTokenInfo, isExpired)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.String.Interpolate (i)
import Data.Aeson (decode)
import Data.Text (Text)

inputTokenInfoRes :: TokenInfoRes
inputTokenInfoRes = TokenInfoRes
  { TIR.idToken      = "abc124"
  , TIR.refreshToken = "xyz789"
  , TIR.expiresIn    = "3600"
  }

testInstantiation :: Test
testInstantiation = TestCase $ do
  let expectedTokenInfo = TokenInfo "abc124" "xyz789" 1003600
      fakeTimeFunc = return 1000000
  actual <- makeTokenInfo fakeTimeFunc inputTokenInfoRes 
  assertEqual "Make TokenInfo instance from given TokenInfoRes"
    expectedTokenInfo
    actual

testDeserialization :: Test
testDeserialization = TestCase $
  let expectedTokenInfo = TokenInfo "abc124" "xyz789" 1003602
      exampleJson = BL.pack [i|
      {
        "idToken":      "abc124",
        "refreshToken": "xyz789",
        "expiresAt":    1003602
      }|]
  in assertEqual "Deserialization of TokenInfo"
    (Just expectedTokenInfo)
    (decode exampleJson)

testExpiration :: Test
testExpiration = TestCase $ do
    assertExpired False "1 sec before: not expired" 1003600
    assertExpired False "just now: not expired"     1003601
    assertExpired True  "1 sec after: expired"      1003602
  where
    sampleTokenInfo = TokenInfo "---" "---" 1003601
    assertExpired :: Bool -> String -> Int -> Assertion
    assertExpired expected message currentTime = do
      result <- isExpired (return currentTime) sampleTokenInfo
      assertEqual message expected result

main :: IO ()
main = runTestTTAndExit $ TestList
  [ testInstantiation
  , testDeserialization
  , testExpiration
  ]