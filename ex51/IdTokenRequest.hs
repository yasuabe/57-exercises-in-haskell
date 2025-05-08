{-# LANGUAGE DeriveGeneric #-}

module IdTokenRequest where

import Data.Aeson (FromJSON(..), ToJSON(..), defaultOptions, genericParseJSON, genericToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data IdTokenRequest = IdTokenRequest
  { email             :: Text
  , password          :: Text
  , returnSecureToken :: Bool
  } deriving (Generic)

instance FromJSON IdTokenRequest where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON IdTokenRequest where
  toJSON = genericToJSON defaultOptions
