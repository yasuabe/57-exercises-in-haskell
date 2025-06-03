-- Ex9: Paint Calculator
--
-- - Prompt for room length and width.
-- - Compute area and divide by 350 (coverage per gallon).
-- - Round up to the next whole gallon.
-- - Use a constant for coverage rate.-- Ex8: Pizza PeopleAndPizza

{-# LANGUAGE LambdaCase, BlockArguments, GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

 module Main where

import Polysemy (Sem, Member, Embed, runM, interpret, embed)
import System.Console.Haskeline (InputT)

import qualified Common.System as S
import Common.App
import Ex09

type App = (InputT IO)

consoleToApp :: Member (Embed App) r => Sem (Console ': r) a -> Sem r a
consoleToApp = interpret \case
  ReadLine  prompt -> embed @App (S.readLine prompt)
  WriteLine msg    -> embed @App (S.putTextLn msg )

main :: IO ()
main = runProgram $ runM . consoleToApp $ program
