{-# LANGUAGE OverloadedStrings #-}

module Spec where

import Ex29 (ruleOf72)
import Test.HUnit

testRuleOf72 :: Test
testRuleOf72 =
  TestCase $
    assertEqual
      "test rule of 72"
      18
      (ruleOf72 4)

main :: IO ()
main =
  runTestTTAndExit $
    TestList [testRuleOf72]