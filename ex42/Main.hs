{-# LANGUAGE OverloadedStrings #-}

--  Ex42: Parsing a Data File
-- 
-- ・Read a file with comma-separated records (no CSV library).
-- ・Each line has: Last, First, Salary.
-- ・Parse lines into records manually.
-- ・Print a table with aligned columns using spaces.
-- ・Format must match the sample output.

module Main where

import Data.Foldable (forM_)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty(..), singleton, (<|), reverse)
import Data.Text as T (Text, unpack, pack, split, length, replicate)
import Prelude hiding (lines, words, unlines, unwords, reverse, head, tail, zipWith3)
import qualified Streamly.FileSystem.File as File
import qualified Streamly.Internal.Unicode.Stream as U
import qualified Streamly.Data.Fold as F (Fold, foldl', toList)
import qualified Streamly.Data.Stream as S (fold)

type Triple a = (a, a, a)
type TableInfo = (NonEmpty (Triple Text), Triple Int)

inputFile :: FilePath
inputFile = "ex42/data/employee.csv"

apply3 :: (a -> b) -> Triple a -> Triple b
apply3 f (a, b, c) = (f a, f b, f c)

zipWith3 :: (a -> b -> c) -> Triple a -> Triple b -> Triple c
zipWith3 f (a1, a2, a3) (b1, b2, b3) = (f a1 b1, f a2 b2, f a3 b3)

toTriple :: [Text] -> Triple Text
toTriple []             = ("", "", "")
toTriple [a]            = (a, "", "")
toTriple [a, b]         = (a, b, "")
toTriple (a : b : c: _) = (a, b, c)

foldToTableInfo :: (Monad m) => F.Fold m Text TableInfo
foldToTableInfo = F.foldl' step initial
  where
  initial :: TableInfo
  initial = let header = ("Last", "First", "Salary")
            in (singleton header, apply3 T.length header)

  step :: TableInfo -> Text -> TableInfo
  step (rows, max3) line =
    let headRow = toTriple $ T.split (==',') line
    in (
      headRow <| rows,
      zipWith3 (\b c -> max b $ T.length c) max3 headRow
    )

displayResult :: TableInfo -> IO ()
displayResult (lines, (w1, w2, w3)) = do
  putRow header
  putRow $ apply3 (`T.replicate` "-") (w1', w2', w3')
  forM_ rows putRow

  where
  header :| rows  = reverse lines
  (w1', w2', w3') = apply3 (+1) (w1, w2, w3)
  pad w str       = str <> T.replicate (w - T.length str) " "
  putRow (v1, v2, v3) = putStrLn
                      $ T.unpack
                      $ pad w1' v1 <> pad w2' v2 <> pad w3' v3

main :: IO ()
main =
  File.readChunks inputFile
  &   U.decodeUtf8Chunks
  &   U.lines F.toList
  &   fmap pack
  &   S.fold foldToTableInfo
  >>= displayResult
