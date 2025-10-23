-- # Ex17: Blood Alcohol Calculator
--
-- - Prompt for weight, gender, alcohol amount, and time since last drink.
-- - Compute BAC using a given formula.
-- - Report whether it's legal to drive (BAC ≥ 0.08 means illegal).
-- - Constraint: Validate that inputs are numeric.

module Main where

import Control.Monad.Free (Free(..))
import System.Console.Haskeline (InputT)

import Common.App (runProgram)
import qualified Common.System as S (putTextLn, readLine)
import Ex17

runConsole :: Console a -> InputT IO a
runConsole (Pure a) = return a
runConsole (Free (WriteLine str next)) = S.putTextLn str >> runConsole next
runConsole (Free (ReadLine str f))     = S.readLine str >>= runConsole . f

main :: IO ()
main = runProgram $ runConsole program
