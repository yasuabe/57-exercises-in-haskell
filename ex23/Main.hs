{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- Ex23: Troubleshooting Car Issues
--
-- ・Guide the user through a series of yes/no questions based on a decision tree.
-- ・Ask only relevant questions based on previous answers.
-- ・Display appropriate troubleshooting advice depending on the answers.
-- ・Do not prompt for all inputs at once; follow the flow of the decision tree.

module Main where

import Data.Function ((&))
import Data.Text (Text, unpack, strip, toLower)
import System.Console.Haskeline (InputT)

import Common.App (runProgram)
import Common.System (putTextLn, repeatUntilValid)

data Tree a = Leaf a
            | Node a (Tree a) (Tree a)

decisionTree :: Tree Text
decisionTree = Node
  "Is the car silent when you turn the key? "
  (Node
    "Are the battery terminals corroded? "
    (Leaf "Clean terminals and try starting again.")
    (Leaf "Replace cables and try again."))
  (Node
    "Does the car make a clicking noise? "
    (Leaf "Replace the battery.")
    (Node
      "Does the car crank up but fail to start? "
      (Leaf "Check spark plug connections.")
      (Node
        "Does the engine start and then die? "
        (Node
          "Does your car have fuel injection? "
          (Leaf "Get it in for service.")
          (Leaf "Check to ensure the choke is opening and closing."))
        (Leaf "---"))))

data YesNo = Yes | No

askYesNo :: Text -> InputT IO YesNo
askYesNo prompt =
  repeatUntilValid convertToYesNo
                   prompt
                   "Enter yes/no or y/n (not case sensitive)."
  where
    convertToYesNo :: Text -> Maybe YesNo
    convertToYesNo t
      | t' == "yes" || t' == "y" = Just Yes
      | t' == "no"  || t' == "n" = Just No
      | otherwise                = Nothing
      where
        t' = t & strip & toLower

iterateTree :: Tree Text -> InputT IO ()
iterateTree (Leaf answer) = putTextLn answer
iterateTree (Node prompt left right) =
  askYesNo prompt >>= \case 
    Yes -> iterateTree left
    No  -> iterateTree right

main :: IO ()
main = runProgram $ iterateTree decisionTree
