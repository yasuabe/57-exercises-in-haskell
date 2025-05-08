{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module AddNoteReqTest where

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.String.Interpolate (i)
import Data.Text (Text)
import Test.HUnit

import AddNoteReq
import GetNotesRes (GetNotesRes(..), NoteRes(..), toNotes)

exampleJson :: BL.ByteString
exampleJson = BL.pack [i|
{
  "date": "2025-04-28",
  "note": "hello"
}|]

testCreation :: Test
testCreation = TestCase $ do
  actual <- noteNow (return 981173106) "n1"
  assertEqual "981173106 is 2001-02-03 04:05:06" (AddNoteReq "2001-02-03" "n1") actual

main :: IO ()
main = runTestTTAndExit $ TestList
  [ testCreation
  ]
