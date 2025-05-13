{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module CommandTest (main) where

import Test.HUnit
import Data.String.Interpolate (i)
import Data.Text as T (Text, words, unwords, strip)
import Command (Command(..), parseText)

testParseCommand :: Test
testParseCommand = TestCase $ do
    assertCommand (Just $ AddTask "abc def") ":add abc def"
    assertCommand (Just $ AddTask "abc def") " :add   abc def"
    assertCommand Nothing                    ":addabc def"

    assertCommand (Just ListTasks)           ":list"
    assertCommand (Just ListTasks)           " :list xyz "
    assertCommand Nothing                    " :listxyz"

    assertCommand (Just $ RemoveTask "1")    ":remove 1"
    assertCommand (Just $ RemoveTask "a")    " :remove   a "
    assertCommand Nothing                    "  :removea"

    assertCommand (Just Exit)                ":exit"
    assertCommand (Just Exit)                " :exit:  some noice  "
    assertCommand Nothing                    "  :exitt"
  where
    assertCommand expected input = 
      assertEqual [i|"#{input}" not matched|] expected (parseText input)

main :: IO ()
main = runTestTTAndExit $ TestList
  [ testParseCommand
  ]