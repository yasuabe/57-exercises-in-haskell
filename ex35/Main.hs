-- # Ex35: Picking a Winner
--
-- ・Prompt for names until a blank line is entered.
-- ・Store non-blank names in a collection.
-- ・Randomly select and print one name as the winner.
-- ・Use a loop for input and a random number generator for selection.
-- ・Exclude blank entries.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.String.Interpolate (i)
import Data.Text as T (Text, null)
import System.Console.Haskeline (InputT)
import System.Random (randomRIO)
import Control.Monad.Loops (unfoldWhileM)

import Common.System (putTextLn, readLine)
import Common.App (runProgram)

type AppType = InputT IO

collectNames :: AppType [Text]
collectNames = unfoldWhileM (not . T.null)
                            (readLine "Enter a name: ")

program :: AppType ()
program = do
  names <- collectNames
  if Prelude.null names then putTextLn "No names were entered."
  else do
    index      <- randomRIO (0, length names - 1)
    let winner =  names !! index
    putTextLn [i|The winner is #{winner}.|]

main :: IO ()
main = runProgram program
