-- Ex54: URL Shortener
-- 
-- ・Create a web app that shortens long URLs.
-- ・Features:
--   ・A form to submit a long URL.
--   ・Generates and stores a short URL (e.g. /abc1234) that redirects to the long one.
--   ・Tracks how many times the short URL is visited.
--   ・Provides a stats page (/abc1234/stats) showing:
--     ・ The short URL
--     ・ The original long URL
--     ・ Visit count
-- ・ Constraints:
--   ・ Must use a persistent, shareable data store (e.g. DB, not memory).
--   ・ Must validate that the input is a valid URL.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Control.Monad.Trans (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Function ((&))
import Data.List (unfoldr)
import Data.Maybe (isJust)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.SQLite.Simple
import GHC.Generics (Generic)
import Network.URI (parseURI)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import Servant

import Common.Function (maybeIf)
import DB (PrimaryKey (UrlId), UrlT (Url), incrementVisitCount, insertUrl, lookupUrl)

-- Types
data ShortenReq where
  ShortenReq :: { longUrl :: Text } -> ShortenReq
  deriving (Show, Eq, Generic)
instance FromJSON ShortenReq

data ShortenRes where
  ShortenRes :: { shortUrl :: Text } -> ShortenRes
  deriving (Show, Eq, Generic)
instance ToJSON ShortenRes

data RedirectRes where
  RedirectRes :: { longUrl :: Text } -> RedirectRes
  deriving (Show, Eq, Generic)
instance ToJSON RedirectRes

data StatsRes = StatsRes
  { shortUrl   :: String
  , longUrl    :: String
  , visitCount :: Int
  } deriving (Show, Eq, Generic)
instance ToJSON StatsRes

type AppEx54 = ReaderT Connection Handler

type API =
       "ex54" :> ReqBody '[JSON] ShortenReq         :> Post '[JSON] ShortenRes
  :<|> "ex54" :> Capture "shortUrl" Text            :> Get  '[JSON] NoContent
  :<|> "ex54" :> Capture "shortUrl" Text :> "stats" :> Get  '[JSON] StatsRes

-- Functions
makeShortURL :: IO Text
makeShortURL = do
  num <- (+ 10000000000) . round <$> getPOSIXTime
  return $ unfoldr (\b -> maybeIf (b > 0) (let (d, m) = divMod b 62 in (base62 !! m, d))) num
         & reverse
         & pack
  where base62 = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

runSqlite :: (Connection -> IO a) -> ReaderT Connection Handler a
runSqlite f = ask >>= (liftIO . f)

shortenUrl :: ShortenReq -> AppEx54 ShortenRes
shortenUrl (ShortenReq longUrl') = do
  _         <- validateUrl longUrl'
  shortUrl' <- liftIO makeShortURL
  _         <- runSqlite $ insertUrl shortUrl' longUrl'
  return $ ShortenRes { shortUrl = shortUrl' }
  where
    validateUrl :: Text -> AppEx54 Text
    validateUrl url =
      if isJust $ parseURI (unpack url)
        then return url
        else throwError $ err400 { errBody = "Invalid URL" }

redirectUrl :: Text -> AppEx54 NoContent
redirectUrl shortUrl' =
  runSqlite (incrementVisitCount shortUrl') >>= \case
    Just longUrl' -> throwError $ err302 { errHeaders = [("Location", encodeUtf8 longUrl')] }
    Nothing       -> throwError $ err404 { errBody = "Not Found" }

viewStats :: Text -> AppEx54 StatsRes
viewStats shortUrl' =
  runSqlite (lookupUrl (UrlId shortUrl')) >>= \case
    Nothing          -> throwError $ err404 { errBody = "Not Found" }
    Just (Url s l c) -> return
                      $ StatsRes { shortUrl   = unpack s
                                 , longUrl    = unpack l
                                 , visitCount = fromIntegral c
                                 }

server :: ServerT API AppEx54
server = shortenUrl
    :<|> redirectUrl
    :<|> viewStats

app :: Connection -> Application
app conn = serve api
         $ hoistServer api (`runReaderT` conn) server

api :: Proxy API
api = Proxy

main :: IO ()
main = do
  conn <- open "ex54/urlShortner.db"
  run 8080 (cors (const $ Just policy) $ app conn)
  where
    policy = simpleCorsResourcePolicy
           { corsRequestHeaders = ["Content-Type"]
           , corsMethods        = ["POST", "GET", "OPTIONS"]
           , corsOrigins        = Nothing
           }
