--  Ex22: Comparing Numbers
--
-- ・ Prompt the user to enter three numbers.
-- ・ If any numbers are the same, exit the program.
-- ・ Otherwise, determine and display the largest number.
-- ・ Do not use built-in functions to find the largest value.

{-# LANGUAGE LambdaCase, BlockArguments, GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

 module Main where

import Polysemy (Sem, Member, Embed, runM, interpret, embed)
import System.Console.Haskeline (InputT)
import Polysemy.State

import Common.App
import qualified Common.System as S
import Ex22

type App = (InputT IO)

runApp :: Member (Embed App) r => Sem (Console ': State [Int] ': r) a -> Sem r a
runApp = evalState ([] :: [Int])
       . interpret \case
          ReadLine  prompt -> embed @App (S.readLine prompt)
          WriteLine msg    -> embed @App (S.putTextLn msg )

main :: IO ()
main = runProgram $ runM $ runApp program
