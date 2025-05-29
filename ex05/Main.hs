-- Ex5: Simple Math
--
-- ・Prompt the user to enter two numbers.
-- ・Convert the input strings to numeric types before performing calculations.
-- ・Calculate the sum, difference, product, and quotient.
-- ・Keep input and output separate from processing logic.
-- ・Use a single output statement with line breaks to display the results.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.String.Interpolate (__i)
import Data.Text (Text)
import Common.App (runProgram)
import Common.Function (maybeIf)
import Common.System (putTextLn, readValid)
import System.Console.Haskeline (InputT)

type AppType = InputT IO

program :: AppType ()
program = do
  n1 <- readNumber "What is the first number? "
  n2 <- readNumber "What is the second number? "

  let sumResult      = n1 + n2
      diffResult     = n1 - n2
      prodResult     = n1 * n2
      quotientResult = maybeIf (n2 /= 0) (n1 / n2)

  putTextLn [__i|
    #{n1} + #{n2} = #{sumResult}
    #{n1} - #{n2} = #{diffResult}
    #{n1} * #{n2} = #{prodResult}
    #{n1} / #{n2} = #{maybe "N/A" show quotientResult}|]
  where
    readNumber :: Text -> AppType Double
    readNumber = (`readValid` "Please enter a valid number.")

main :: IO ()
main = runProgram program