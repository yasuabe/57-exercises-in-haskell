{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module TokenInfo where

import Data.Aeson ( FromJSON, ToJSON )
import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified TokenInfoRes as TI
import Control.Monad.Catch (throwM)
import Control.Exception (Exception)

data TokenInfo = TokenInfo
  { idToken      :: Text
  , refreshToken :: Text
  , expiresAt    :: Int
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data TokenInfoException = InvalidExpiresInValue
  deriving (Show, Eq)

instance Exception TokenInfoException

makeTokenInfo :: IO Int -> TI.TokenInfoRes -> IO TokenInfo
makeTokenInfo timeFunc (TI.TokenInfoRes idToken' refreshToken' expiresIn) = do
  let expiresInSeconds = readMaybe (T.unpack expiresIn) :: Maybe Int
  case expiresInSeconds of
    Just expIn -> do
      currentTime <- timeFunc
      let expiresAt' = currentTime + expIn
      return $ TokenInfo idToken' refreshToken' expiresAt'
    Nothing -> throwM InvalidExpiresInValue

isExpired :: IO Int -> TokenInfo -> IO Bool
isExpired timeFunc tokenInfo = do
  currentTime <- timeFunc
  return $ expiresAt tokenInfo < currentTime

makeTokenInfoIO :: TI.TokenInfoRes -> IO TokenInfo
makeTokenInfoIO = makeTokenInfo (round <$> getPOSIXTime)

isExpiredIO :: TokenInfo -> IO Bool
isExpiredIO = isExpired (round <$> getPOSIXTime)