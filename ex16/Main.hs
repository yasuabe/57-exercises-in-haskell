-- # Ex16: Legal Driving Age
--
-- - Prompt the user for their age.
-- - Compare it to the legal driving age (16).
-- - Output a single message:
--   - If 16 or older → “You are old enough to legally drive.”
--   - If under 16 → “You are not old enough to legally drive.”
-- - Use a ternary operator if available, or if/else with a single print statement.

module Main where

import Control.Monad.Free
import System.Console.Haskeline (InputT)

import Common.App (runProgram)
import qualified Common.System as S (putTextLn, readLine)
import Ex16

runConsole :: Console a -> InputT IO a
runConsole (Pure a) = return a
runConsole (Free (WriteLine str next)) = S.putTextLn str >> runConsole next
runConsole (Free (ReadLine str f))     = S.readLine str >>= runConsole . f

main :: IO ()
main = runProgram $ runConsole program
