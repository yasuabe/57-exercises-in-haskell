{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- ## Ex41: Name Sorter
-- ・Read a list of names from a file.
-- ・Sort the names alphabetically.
-- ・Output:
--   ・ Total number of names.
--   ・ A separator line.
--   ・ The sorted names.
-- ・Do not hard-code the number of names.

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.List (sort)
import Data.String.Interpolate (i)
import Data.Text (Text, pack, unpack)
import GHC.IO.Handle.FD (withFile)
import System.Console.Haskeline (InputT)
import System.IO (IOMode(..), hPutStrLn)

import Common.App (runProgram)

namesPath :: FilePath
namesPath = "ex41/names.txt"

sortedPath :: FilePath
sortedPath = "ex41/output/sorted.txt"

readNames :: IO [Text]
readNames = do
  content <- readFile namesPath
  return $ map pack $ lines content

writeReport :: (Text -> IO ()) -> [Text] -> IO ()
writeReport writeFunc names = do
  let totalNames = length names
  writeFunc [i|Total of #{totalNames} names: |]
  writeFunc "--------------------"
  mapM_ writeFunc names

program :: InputT IO ()
program = do
  names <- liftIO readNames
  let sortedNames = sort names
  liftIO $ withFile sortedPath WriteMode $ \h -> do
    writeReport (hPutStrLn h . unpack) sortedNames

main :: IO ()
main = runProgram program
