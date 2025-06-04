{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs
           , FlexibleContexts, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables #-}

 module Ex22 where

import Data.Function ((&))
import Data.String.Interpolate (i)
import Data.Text as T (Text, null, pack, unpack, strip)
import Text.Read (readMaybe)
import Polysemy (Sem, makeSem, Member)
import Polysemy.State

data Console m r where
  ReadLine  :: Text -> Console m Text
  WriteLine :: Text -> Console m ()

makeSem ''Console

data Input = Number Int | Exit

readNumber :: (Member Console r, Member (State [Int]) r) => Sem r Input
readNumber = do
  nums  <- get
  input <- strip <$> readLine [i|Enter a number (#{length (nums :: [Int])}): |]
  (readMaybe . unpack) input & \case
    Just n | n `elem` nums -> onInvalid "Duplicate input."
    Just n                 -> return $ Number n
    _ | T.null input       -> return Exit
    _                      -> onInvalid "Invalid input."
  where onInvalid errMsg = writeLine errMsg >> readNumber

selectMaxInt :: (Member Console r, Member (State [Int]) r) => Maybe Int -> Sem r (Maybe Int)
selectMaxInt mbInt = readNumber >>= \case
  Exit     -> return mbInt
  Number n -> selectMaxInt $ Just $ maybe n (max n) mbInt

program :: (Member Console r, Member (State [Int]) r) => Sem r ()
program = selectMaxInt Nothing >>= \case
  Just maxVal -> writeLine $ "The largest number is: " <> pack (show maxVal)
  Nothing     -> writeLine "No numbers were entered."
