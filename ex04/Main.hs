-- Ex4: Mad Lib
-- ・ Prompt the user to enter a noun, a verb, an adjective, and an adverb.
-- ・ Create a story using the inputs.
-- ・ Use string interpolation or substitution to build the output.
-- ・ Use a single output statement to display the story.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.String.Interpolate (i)
import Common.App (runProgram)
import Common.System (readLine, putTextLn)
import System.Console.Haskeline (InputT)

program :: InputT IO ()
program = do
  n   <- readLine "Enter a noun: "
  v   <- readLine "Enter a verb: "
  adj <- readLine "Enter an adjective: "
  adv <- readLine "Enter an adverb: "

  putTextLn [i|Do you #{v} your #{adj} #{n} #{adv}? That's hilarious!|]

main :: IO ()
main = runProgram program