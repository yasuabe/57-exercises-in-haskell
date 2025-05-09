{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module TokenInfoRes where

import Data.Aeson
import Data.Aeson.Types (Object, Parser, (.:), parseMaybe)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Text (Text)
import GHC.Generics (Generic)

data TokenInfoRes = TokenInfoRes
  { idToken      :: Text
  , refreshToken :: Text
  , expiresIn    :: Text
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

decodeSnakeCaseRes :: ByteString -> Maybe TokenInfoRes
decodeSnakeCaseRes s =
  case eitherDecode s of
    Right (Object obj) -> parseMaybe parseSnakeCaseTokenInfoRes obj
    _                  -> Nothing
  where
    parseSnakeCaseTokenInfoRes obj = TokenInfoRes
      <$> obj .: "id_token"
      <*> obj .: "refresh_token"
      <*> obj .: "expires_in"
