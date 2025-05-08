{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module TokenInfoResTest where

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.String.Interpolate (i)
import Test.HUnit

import TokenInfoRes (TokenInfoRes(..), decodeSnakeCaseTokenInfoRes)

exampleJson :: BL.ByteString
exampleJson = BL.pack [i|
{
  "idToken":      "abc124",
  "refreshToken": "xyz789",
  "expiresIn":    "3600"
}|]

exampleSnakeCaseJson :: BL.ByteString
exampleSnakeCaseJson = BL.pack [i|
{
  "id_token":      "abc124",
  "refresh_token": "xyz789",
  "expires_in":    "3600"
}|]

expectedTokenInfoRes :: TokenInfoRes
expectedTokenInfoRes = TokenInfoRes
  { idToken      = "abc124"
  , refreshToken = "xyz789"
  , expiresIn    = "3600"
  }

testDeserialization :: Test
testDeserialization = TestCase $
  assertEqual "Deserialization of TokenInfoRes"
    (Just expectedTokenInfoRes)
    (decode exampleJson)

testSnakeCaseDeserialization :: Test
testSnakeCaseDeserialization = TestCase $
  assertEqual "Deserialization of TokenInfoRes with snake_case JSON"
    (Just expectedTokenInfoRes)
    (decodeSnakeCaseTokenInfoRes exampleSnakeCaseJson)

main :: IO ()
main = runTestTTAndExit $ TestList
  [ testDeserialization
  , testSnakeCaseDeserialization
  ]