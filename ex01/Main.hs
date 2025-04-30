-- Ex1: Saying Hello
--
-- - Prompt the user to enter their name.
-- - Create a greeting message using string concatenation.
-- - Print the greeting.
-- - Keep input, concatenation, and output as separate steps.

module Main where
import System.IO (hFlush, stdout)

main :: IO ()
main = do
    -- Step 1: input
    putStr "What is your name? "
    hFlush stdout
    name <- getLine

    -- Step 2: concatenation
    let greeting = "Hello, " ++ name ++ ", nice to meet you!"

    -- Step 3: output
    putStrLn greeting
