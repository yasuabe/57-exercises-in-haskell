{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module GetNotesRes where

import Data.Aeson ( FromJSON, ToJSON )
import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.HashMap.Strict as HM
import qualified Note as N
import Data.List (sortBy)
import Data.Function ((&))

data NoteRes = NoteRes
  { date :: Text
  , note :: Text
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

type GetNotesRes = HM.HashMap Text NoteRes

toNotes :: GetNotesRes -> [N.Note]
toNotes notesMap
  = map toNote (HM.elems notesMap)
  & sortBy (\a b -> compare (N.date a) (N.date b))
  where
    toNote :: NoteRes -> N.Note
    toNote (NoteRes date note) = N.Note date note
