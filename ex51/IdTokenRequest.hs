{-# LANGUAGE DeriveGeneric #-}

module IdTokenRequest where

import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON, defaultOptions)

data IdTokenRequest = IdTokenRequest
  { email             :: Text
  , password          :: Text
  , returnSecureToken :: Bool
  } deriving (Generic)

-- Define FromJSON instance
instance FromJSON IdTokenRequest where
  parseJSON = genericParseJSON defaultOptions

-- Define ToJSON instance
instance ToJSON IdTokenRequest where
  toJSON = genericToJSON defaultOptions
