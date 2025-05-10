{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- Ex23: Troubleshooting Car Issues
--
-- ・Guide the user through a series of yes/no questions based on a decision tree.
-- ・Ask only relevant questions based on previous answers.
-- ・Display appropriate troubleshooting advice depending on the answers.
-- ・Do not prompt for all inputs at once; follow the flow of the decision tree.

module Main where

import Data.Text (Text)
import System.Console.Haskeline (InputT)

import Common.App (runProgram)
import Common.System (YesNo(..), askYesNo, putTextLn)

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

iterateTree :: Tree Text -> InputT IO ()
iterateTree (Leaf answer) = putTextLn answer
iterateTree (Node prompt left right) =
  askYesNo prompt >>= \case 
    Yes -> iterateTree left
    No  -> iterateTree right

main :: IO ()
main = runProgram $ iterateTree decisionTree
