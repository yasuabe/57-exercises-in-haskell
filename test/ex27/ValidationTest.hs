{-# LANGUAGE OverloadedStrings #-}

module ValidationTest where

import Data.Validation ( Validation(Failure, Success) )
import Test.HUnit

import Validation

testValidationValid :: Test
testValidationValid = TestCase $
  let result = validateInput( "John"
                            , "Doe"
                            , "12345"
                            , "AB-1234"
                            )
  in assertBool "" $ case result of
                      Success _ -> True
                      Failure _ -> False

testValidationInvalid :: Test
testValidationInvalid = TestCase $
  let result = validateInput( "J"
                            , ""
                            , "ABCDE"
                            , "A12-1234"
                            )
  in case result of
      Success _      -> assertFailure "Expected a Failure but got a Success"
      Failure errors -> assertEqual
        "`errors` should contain exactly 4 error messages"
        errors
        [ "\"J\" is not a valid first name. It is too short."
        , "The last name must be filled in."
        , "ZIP code must be numeric."
        , "A12-1234 is not a valid ID."
        ]

main :: IO ()
main = runTestTTAndExit $ TestList
  [ testValidationValid
  , testValidationInvalid
  ]
