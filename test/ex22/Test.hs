{-# LANGUAGE LambdaCase, BlockArguments, GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables, OverloadedStrings #-}

module Test where

import Data.Maybe (fromMaybe)
import Data.Text as T (Text, isInfixOf)
import Polysemy (Sem, reinterpret2, runM)
import Polysemy.Input (input, runInputList)
import Polysemy.Output (output, runOutputMonoid)
import Polysemy.State (State, runState)
import Test.HUnit hiding (State)

import Ex22

runMock :: [Text] -> Sem (Console ': State [Int] ': r) a -> Sem r ([Text], a)
runMock i = fmap snd
          . runState []
          . runOutputMonoid pure
          . runInputList i
          . reinterpret2 \case
              ReadLine  _   -> Data.Maybe.fromMaybe "" <$> input
              WriteLine msg -> output msg

testProgram :: [Text] -> [Text] -> Test
testProgram inputs expects= TestCase $ do
  let (_, (outputs, _)) = runM $ runMock inputs program :: (Text, ([Text], ()))
  assertBool
    ("inputs: "++ show inputs) $
    all (\e -> any (T.isInfixOf e) outputs) expects

main :: IO ()
main = runTestTTAndExit $ TestList
     --            | inputs   | expects
     [ testProgram ["2", "3"] ["3"]
     , testProgram ["3", "3"] ["Duplicate"]
     , testProgram ["x", "3"] ["Invalid"]
     , testProgram [""]       ["No numbers were entered."]
     ]
