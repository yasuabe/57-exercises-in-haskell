{-# LANGUAGE LambdaCase, BlockArguments, GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables, OverloadedStrings #-}

module Test where

import Data.Maybe (fromMaybe)
import Data.Text as T (Text, isInfixOf)
import Polysemy (Sem, reinterpret2, runM)
import Polysemy.Input (input, runInputList)
import Polysemy.Output (output, runOutputMonoid)
import Test.HUnit

import Ex09

runMock :: [Text] -> Sem (Console ': r) a -> Sem r ([Text], a)
runMock i = runOutputMonoid pure
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
     --            | inputs        | expects
     [ testProgram ["175", "2"]    ["1 gallon", "350 square feet"]
     , testProgram ["176", "2"]    ["2 gallon", "352 square feet"]
     , testProgram ["1", "-", "1"] ["1 gallon", "Invalid", "1 square feet"]
     ]
