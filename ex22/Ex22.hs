{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs
           , FlexibleContexts, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables #-}

 module Ex22 where

import Data.Function ((&))
import Data.String.Interpolate (i)
import Data.Text as T (Text, null, unpack, strip)
import Text.Read (readMaybe)
import Polysemy (Sem, makeSem, Members)
import Polysemy.State as PS (State, get, put)

data Console m r where
  ReadLine  :: Text -> Console m Text
  WriteLine :: Text -> Console m ()

makeSem ''Console

readNumber :: Members '[Console, State [Int]] r => Sem r (Maybe Int)
readNumber = do
  nums  <- get
  input <- strip <$> readLine [i|Enter a number (#{length (nums :: [Int]) + 1}): |]
  (readMaybe . unpack) input & \case
    Just n | n `elem` nums -> onInvalid "Duplicate input."
    num@(Just n)           -> do _ <- put (n : nums); return num
    _ | T.null input       -> return Nothing
    _                      -> onInvalid "Invalid input."
  where onInvalid errMsg = writeLine errMsg >> readNumber

selectMaxInt :: Members '[Console, State [Int]] r => Maybe Int -> Sem r (Maybe Int)
selectMaxInt mbInt = readNumber >>= \case
  Just n  -> selectMaxInt $ Just $ maybe n (max n) mbInt
  Nothing -> return mbInt

program :: Members '[Console, State [Int]] r => Sem r ()
program = selectMaxInt Nothing >>= \case
  Just maxNum -> writeLine [i|The largest number is: #{maxNum}|] 
  Nothing     -> writeLine "No numbers were entered."
