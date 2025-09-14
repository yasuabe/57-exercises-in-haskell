-- Ex19: BMI Calculator
--
-- - Prompt the user for their height (in inches) and weight (in pounds).
-- - Calculate BMI using:
-- - ` bmi = (weight / (height × height)) × 703 ``
-- - Display:
--   - “You are within the ideal weight range.” if BMI is 18.5–25.
--   - Otherwise, indicate if the user is underweight or overweight and advise seeing a doctor.
-- - Input must be numeric—reject non-numeric input.
module Main where

import Control.Monad.Free
import System.Console.Haskeline (InputT)

import Common.App (runProgram)
import qualified Common.System as S (putTextLn, readLine)
import Ex19

runConsole :: Console a -> InputT IO a
runConsole (Pure a) = return a
runConsole (Free (WriteLine str next)) = S.putTextLn str >> runConsole next
runConsole (Free (ReadLine str f))     = S.readLine str >>= runConsole . f

main :: IO ()
main = runProgram $ runConsole program
