{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Validation where

import Data.Validation ( Validation(..) )
import Data.String.Interpolate (i)
import Data.Text as T (Text, length, null, strip)
import Text.Regex.TDFA ((=~))

type Input = (Text, Text, Text, Text)
type Errors = [Text]
type ValidationResult a = Validation Errors a

buildValidator :: (Text -> Bool) -> (Text -> Text -> Text) -> Text -> Text -> ValidationResult Text
buildValidator judgeFn failureFn fieldName input =
  if judgeFn input' then Success input'
  else Failure [failureFn fieldName input']
  where input' = T.strip input

validateNonEmpty :: Text -> Text -> ValidationResult Text
validateNonEmpty = buildValidator
  (not . T.null)
  (\fieldName _ -> [i|The #{fieldName} must be filled in.|])

validateMinLength :: Text -> Text -> ValidationResult Text
validateMinLength = buildValidator
  ((>= 2) . T.length)
  (\fieldName input -> [i|"#{input}" is not a valid #{fieldName}. It is too short.|])

validateName :: Text -> Text -> ValidationResult Text
validateName fieldName input = 
  case validateNonEmpty fieldName input of
    Success _   -> validateMinLength fieldName input
    Failure err -> Failure err

validateZipCode :: Text -> Text -> ValidationResult Text
validateZipCode = buildValidator
  (=~ ("^[0-9]+$" :: String))
  (\fieldName _ -> [i|#{fieldName} must be numeric.|])

validateEmployeeID :: Text -> Text -> ValidationResult Text
validateEmployeeID = buildValidator
  (=~ ("^[A-Z]{2}-[0-9]{4}$" :: String))
  (\fieldName input -> [i|#{input} is not a valid #{fieldName}.|])

validateInput :: (Text, Text, Text, Text) -> ValidationResult (Text, Text, Text, Text)
validateInput (firstName, lastName, zipCode, empID) =
  (,,,) <$> validateName       "first name" firstName
        <*> validateName       "last name"  lastName
        <*> validateZipCode    "ZIP code"   zipCode
        <*> validateEmployeeID "ID"         empID
