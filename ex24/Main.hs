{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- Ex24: Anagram Checker
-- ・Prompt the user to enter two strings.
-- ・Check if the strings are anagrams.
-- ・Use a function called isAnagram that takes two strings and returns true or false.
-- ・Ensure both strings are the same length before checking further.
-- ・Display whether the two strings are anagrams.

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Data.String.Interpolate (i)
import Data.Text as T (Text, length, strip)

import Common.App (AppType, run)
import Common.Function (maybeIf)
import Common.System (promptInput, repeatUntilValid)
import Ex24 (isAnagram)

program :: AppType ()
program = do
  putTextT "Enter two strings and I'll tell you if they are anagrams:"
  first  <- promptInput "Enter the first string: " & liftIO
  second <- getSecondString first & liftIO

  let notWord = if isAnagram first second then "" else "not " :: Text

  putTextT [i|"#{first}" and "#{second}" are #{notWord}anagrams.|]
  where
    putTextT = liftIO . putStrLn

    getSecond first text =
      let trimmed = T.strip text
      in maybeIf (T.length first == T.length trimmed) trimmed

    getSecondString first = repeatUntilValid
      (getSecond first)
      "Enter the second string: "
      "The second string must be the same length as the first. Please try again."

main :: IO ()
main = run program
