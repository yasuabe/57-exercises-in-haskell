{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
-- Ex46: Word Frequency Finder
-- ・Read a text file.
-- ・Count word frequencies.
-- ・Display a histogram using * to show counts.
-- ・Sort output from most frequent to least frequent.

module Main where

import Conduit
import Data.Foldable (for_)
import qualified Data.HashTable.IO as HIO
import qualified Data.Text as T
import Data.String.Interpolate(i)
import Data.List (sortBy)
import Data.Function (on)

type Counter = HIO.BasicHashTable T.Text Int

wordsFile :: FilePath
wordsFile = "ex46/data/words.txt"

countWords :: Counter -> ConduitT () Void (ResourceT IO) ()
countWords ht
  =  sourceFile wordsFile
  .| decodeUtf8LenientC
  .| concatMapC T.words
  .| mapM_C (liftIO . increment ht)
  where
    increment ht' w = HIO.lookup ht' w >>= HIO.insert ht' w . maybe 1 (+1)

printHistogram :: [(T.Text, Int)] -> IO ()
printHistogram counts =
  for_ counts' $ \(word, count) -> 
    let stars   = replicate count '*'
        padding = replicate (maxLen - T.length word) ' '
    in putStrLn [i|#{word }: #{padding}#{stars}|]
  where
      counts' = sortBy (flip compare `on` snd) counts
      maxLen  = maximum (map (T.length . fst) counts')

main :: IO ()
main = do
  ht <- HIO.new :: IO Counter
  runConduitRes $ countWords ht
  HIO.toList ht >>= printHistogram 
