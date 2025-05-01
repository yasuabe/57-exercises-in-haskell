-- Ex3: Printing Quotes
--
-- - Prompt the user to enter a quote.
-- - Prompt the user to enter the author of the quote.
-- - Display the author and quote using escaped quotation marks.
-- - Use string concatenation, not interpolation or substitution.
-- - Use a single output statement for the result.
module Main where
import Common.Util (promptInput)
import Ex03 (makeOutput)

main :: IO ()
main = do
  quote  <- promptInput "What is the quote? "
  author <- promptInput "Who said it? "
  let output = makeOutput author quote
  putStrLn output
