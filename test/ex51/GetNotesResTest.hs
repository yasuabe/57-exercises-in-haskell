{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module GetNotesResTest where

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.String.Interpolate (i)
import Data.Text (Text)
import Debug.Trace (trace)
import qualified Data.HashMap.Strict as HM
import Test.HUnit
    ( assertEqual,
      assertFailure,
      runTestTTAndExit,
      Test(TestList, TestCase) )

import GetNotesRes (GetNotesRes(..), NoteRes(..), toNotes)
import Note (Note(Note))

exampleJson :: BL.ByteString
exampleJson = BL.pack [i|
{
  "key1": {
    "date": "2025-04-28",
    "note": "hello"
  },
  "key2": {
    "date": "2025-04-29",
    "note": "goodbye"
  }
}|]

testDeserialization :: Test
testDeserialization = TestCase $
  let expectedNotes = HM.fromList
        [ ("key1", NoteRes "2025-04-28" "hello")
        , ("key2", NoteRes "2025-04-29" "goodbye")
        ]
  in case (decode exampleJson :: Maybe GetNotesRes) of
    Just actual -> assertEqual "deserialize Firebase response and make array of notes" expectedNotes actual
    Nothing     -> assertFailure "Failed to decode GetNotesRes JSON"

testToNotes :: Test
testToNotes = TestCase $
  let inputNotes = HM.fromList
        [ ("key1", NoteRes "2025-04-29" "goodbye")
        , ("key2", NoteRes "2025-04-28" "hello")
        ]
      expected =
        [ Note "2025-04-28" "hello"
        , Note "2025-04-29" "goodbye"
        ]
  in assertEqual "Successfully converted map of NoteRes to list of Note" expected (toNotes inputNotes)

main :: IO ()
main = runTestTTAndExit $ TestList
  [ testDeserialization
  , testToNotes
  ]
