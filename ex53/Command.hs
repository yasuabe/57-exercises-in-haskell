{-# LANGUAGE OverloadedStrings #-}

module Command (Command(..), parseText) where

import Data.Text (Text)
import Text.Regex.TDFA ((=~))
import Data.Text as T (Text, words, unwords, strip)

data Command
  = AddTask Text
  | ListTasks
  | RemoveTask Text
  | Exit
  deriving (Show, Eq)

parseText:: Text -> Maybe Command
parseText input =
  case input' =~ regex :: (Text, Text, Text, [Text]) of
    (_, _, _, ["add",    task]) -> Just $ AddTask task
    (_, _, _, ["list",   _   ]) -> Just ListTasks
    (_, _, _, ["remove", id  ]) -> Just $ RemoveTask id
    (_, _, _, ["exit",   _   ]) -> Just Exit
    _                           -> Nothing
    where
      regex = "[ \t]*:([a-z]+)\\b[ \t]*(.*)[ \t]*$" :: Text
      input' :: Text
      input' = T.strip input