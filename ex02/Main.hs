{-# LANGUAGE QuasiQuotes #-}

-- Ex2: Counting the Number of Characters
--
-- - Prompt the user to enter an input string.
-- - Determine the number of characters using a built-in function.
-- - Output the original string and its character count.
-- - Use a single output statement to construct the output.
module Main where
import Data.String.Interpolate (i)
import Common.Util (promptInput)

main :: IO ()
main = do
  input <- promptInput "What is the input string? "
  putStrLn [i|#{input} has #{length input} characters.|]
