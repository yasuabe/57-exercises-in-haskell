{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}

module Config where

import Data.Aeson (FromJSON(..), defaultOptions, genericParseJSON)
import Data.String.Interpolate (i)
import Data.Text (Text)
import GHC.Generics (Generic)

data Config = Config
  { projectId :: Text
  , region    :: Text
  , apiKey    :: Text
  , email     :: Text
  , password  :: Text
  } deriving (Generic)

instance FromJSON Config where
  parseJSON = genericParseJSON defaultOptions

firebaseUrl :: Config -> Text -> Text
firebaseUrl config idToken = case region config of
  "us-central1" -> [i|https://#{projectId config}-default-rtdb.firebaseio.com/notes.json?auth=#{idToken}|]
  _             -> [i|https://#{projectId config}-default-rtdb.#{region config}.firebasedatabase.app/notes.json?auth=#{idToken}|]
