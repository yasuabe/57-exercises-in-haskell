{-# LANGUAGE OverloadedStrings #-}

module Command (Command(..), parseText) where

import Data.Text (Text)
import Text.Regex.TDFA ((=~))
import Data.Text as T (strip)

data Command
  = AddTask Text
  | ListTasks
  | RemoveTask Text
  | Exit
  deriving (Show, Eq)

parseText:: Text -> Maybe Command
parseText input =
  case input' =~ pattern :: (Text, Text, Text, [Text]) of
    (_, _, _, ["add",    ""    ]) -> Nothing
    (_, _, _, ["add",    task  ]) -> Just $ AddTask task
    (_, _, _, ["list",   _     ]) -> Just ListTasks
    (_, _, _, ["remove", ""    ]) -> Nothing
    (_, _, _, ["remove", taskId]) -> Just $ RemoveTask taskId
    (_, _, _, ["exit",   _     ]) -> Just Exit
    _                             -> Nothing
    where
      pattern = "[ \t]*:([a-z]+)\\b[ \t]*(.*)[ \t]*$" :: Text
      input'  = T.strip input
