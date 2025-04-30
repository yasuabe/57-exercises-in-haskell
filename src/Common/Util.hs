module Common.Util (promptInput) where

import System.IO (hFlush, stdout)

-- A helper function to prompt the user and read input
promptInput :: String -> IO String
promptInput promptText = do
  putStr promptText
  hFlush stdout
  getLine

