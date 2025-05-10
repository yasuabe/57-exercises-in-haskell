{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

-- Ex40: Filtering Records
-- ・ Create a program to filter employee records.
-- ・ Search is based on first or last name containing a given substring.
-- ・ Display matching records in a formatted table.
-- ・ Data should be stored in an array of maps (or equivalent structure).

import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Data.Text as T (Text, isInfixOf, length, replicate)
import Data.String.Interpolate (i)
import System.Console.Haskeline (InputT)
import Common.System (putTextLn, readLine)
import Common.App (runProgram)

import Ex40

type Row = (Text, Text, Text)

fromTexts :: Row -> MaxTriple
fromTexts = MaxTriple . (\(a, b, c) -> (T.length a, T.length b, T.length c))

data Employee = Employee
  { firstName      :: Text
  , lastName       :: Text
  , position       :: Text
  , separationDate :: Maybe Text
  } deriving (Show)

employees :: [Employee]
employees = -- First Name | Last Name  | Position          | Separation date |
  [ Employee  "John"      "Johnson"    "Manager"           (Just "2016-12-31")
  , Employee  "Tou"       "Xiong"      "Software Engineer" (Just "2016-10-05")
  , Employee  "Michaela"  "Michaelson" "District Manager"  (Just "2015-12-19")
  , Employee  "Jake"      "Jacobson"   "Programmer"        Nothing
  , Employee  "Jacquelyn" "Jackson"    "DBA"               Nothing
  , Employee  "Sally"     "Weber"      "Web Developer"     (Just "2015-12-18")
  ]

filterEmployees :: Text -> [Employee] -> [Employee]
filterEmployees s = filter (\e -> s `isInfixOf` (firstName e <> lastName e))

displayResults :: [Employee] -> InputT IO ()
displayResults []   = putTextLn "No results found."
displayResults emps = mapM_ printRow allRows
  where
    toRow e = ( " " <> firstName e <> " " <> lastName e
              , " " <> position e
              , " " <> fromMaybe "N/A" (separationDate e)
              )
    headerRow  = (" Name", " Position", " Separation date")
    recordRows = map toRow emps
    allRows    = headerRow :| separator : recordRows
      where hyphens n = T.replicate (n + 1) "-"
            separator = (hyphens w1, hyphens w2, hyphens w3)

    MaxTriple(w1, w2, w3) = foldMap fromTexts (headerRow :| recordRows)

    printRow (c1, c2, c3) = putTextLn [i|#{pad w1 c1}|#{pad w2 c2}|#{pad w3 c3}|]
      where pad width text = text <> T.replicate (width - T.length text + 1) " "

program :: InputT IO ()
program = do
  searchStr <- readLine "Enter a search string: "
  employees & filterEmployees searchStr
            & displayResults

main :: IO ()
main = runProgram program
