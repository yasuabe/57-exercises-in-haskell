{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs
           , FlexibleContexts, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables #-}

 module Ex22 where

import Data.Function ((&))
import Data.String.Interpolate (i)
import Data.Text as T (Text, null, unpack, strip)
import Text.Read (readMaybe)
import Polysemy (Sem, makeSem, Member)
import Polysemy.State as PS (State, get, put)

data Console m r where
  ReadLine  :: Text -> Console m Text
  WriteLine :: Text -> Console m ()

makeSem ''Console

data Input = Number Int | Exit

readNumber :: (Member Console r, Member (State [Int]) r) => Sem r Input
readNumber = do
  nums  <- get
  input <- strip <$> readLine [i|Enter a number (#{length (nums :: [Int]) + 1}): |]
  (readMaybe . unpack) input & \case
    Just n | n `elem` nums -> onInvalid "Duplicate input."
    Just n                 -> do _ <- put (n : nums); return $ Number n
    _ | T.null input       -> return Exit
    _                      -> onInvalid "Invalid input."
  where onInvalid errMsg = writeLine errMsg >> readNumber

selectMaxInt :: (Member Console r, Member (State [Int]) r) => Maybe Int -> Sem r (Maybe Int)
selectMaxInt mbInt = readNumber >>= \case
  Number n -> selectMaxInt $ Just $ maybe n (max n) mbInt
  Exit     -> return mbInt

program :: (Member Console r, Member (State [Int]) r) => Sem r ()
program = selectMaxInt Nothing >>= \case
  Just maxNum -> writeLine [i|The largest number is: #{maxNum}|] 
  Nothing     -> writeLine "No numbers were entered."
