-- Ex15: Password Validation
-- 
-- ・Prompt the user for a password.
-- ・Compare the input with a hardcoded known password.
-- ・If it matches (case-sensitive), print “Welcome!”
-- ・Otherwise, print “I don’t know you.”
-- ・Use an if/else statement for the logic.-- Ex14: Tax Calculator
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Extra (ifM)
import Data.Functor ((<&>))
import Data.Text (Text)
import System.Console.Haskeline (InputT)

import Common.App (runProgram)
import Common.System (putTextLn, readLine)

type AppType = InputT IO

correctPassword :: Text
correctPassword = "abc$123"

program :: AppType ()
program =
  ifM(readLine "Please enter your password: " <&> (correctPassword ==))
     (putTextLn "Welcome!")
     (putTextLn "I don't know you.")

main :: IO ()
main = runProgram program
