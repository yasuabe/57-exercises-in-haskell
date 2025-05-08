{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module AddNoteReq where

import Data.Aeson (FromJSON, ToJSON)
import Data.Function ((&))
import Data.Text (Text, pack)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import GHC.Generics (Generic)

data AddNoteReq = AddNoteReq
  { date :: Text
  , note :: Text
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

noteNow :: IO Int -> Text -> IO AddNoteReq
noteNow timeFunc note' = do
  currentTime <- timeFunc
  let formatted = fromIntegral currentTime
        & posixSecondsToUTCTime
        & formatTime defaultTimeLocale "%Y-%m-%d"
        & pack
  return $ AddNoteReq formatted note'

noteNowIO :: Text -> IO AddNoteReq
noteNowIO = noteNow (round <$> getPOSIXTime)
