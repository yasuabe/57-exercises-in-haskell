module Note where

import Data.Text (Text)

data Note = Note
  { date :: Text
  , note :: Text
  } deriving (Show, Eq)