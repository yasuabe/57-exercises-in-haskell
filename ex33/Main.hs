{-# LANGUAGE OverloadedStrings #-}

-- Ex33: Magic 8 Ball
-- ・ Create a Magic 8 Ball game.
-- ・ Prompt user for a question.
-- ・ Randomly reply with one of:
--   ・“Yes”
--   ・“No”
--   ・“Maybe”
--   ・“Ask again later”
-- ・ Use a list (array) and a random number generator to select the response.

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Data.Text (Text)
import System.Console.Haskeline (InputT)
import System.Random (randomRIO)

import Common.System (putTextLn, readLine)
import Common.App (runProgram)

answers :: [Text]
answers = ["Yes", "No", "Maybe", "Ask again later"]

pickAnswer :: IO Text
pickAnswer = do
  index <- randomRIO (0, length answers - 1)
  return $ answers !! index

program :: InputT IO ()
program = do
  _      <- readLine "What's your question? "
  answer <- pickAnswer & liftIO
  putTextLn answer

main :: IO ()
main = runProgram program
