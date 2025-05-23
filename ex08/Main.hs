-- Ex8: Pizza PeopleAndPizza
-- 
-- ・ Prompt the user for the number of people, pizzas, and slices per pizza.
-- ・ Calculate the total number of slices.
-- ・ Determine how many slices each person gets evenly.
-- ・ Calculate and display any leftover slices.
-- ・ Output the distribution results clearly.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.String.Interpolate (i)
import System.Console.Haskeline (InputT)
import Common.App (runProgram)
import Common.System (putTextLn, repeatUntilValid, toNonNegativeInt)

type AppType = InputT IO

slicesPerPizza :: Int
slicesPerPizza  = 8

program :: AppType ()
program = do
  people <- readNonNega "How many people? "             "invalid number of people."
  pizzas <- readNonNega "How many pizzas do you have? " "invalid number of pizzas."

  let (slices, leftover)= (pizzas * slicesPerPizza) `divMod` people

  putTextLn [i|#{people} people with #{pizzas} pizzas|]
  putTextLn [i|Each person gets #{slices} slices of pizza.|]
  putTextLn [i|There are #{leftover} slices left over.|]

  where
    readNonNega = repeatUntilValid toNonNegativeInt

main :: IO ()
main = runProgram program
