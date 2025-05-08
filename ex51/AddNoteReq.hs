{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module AddNoteReq where
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, getPOSIXTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Aeson ( FromJSON, ToJSON )
import GHC.Generics (Generic)
import Data.Text (Text, pack)
import Data.Function ((&))

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
  