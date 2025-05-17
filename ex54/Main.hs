-- Ex54: URL Shortener
-- 
-- ・Create a web app that shortens long URLs (like goo.gl).
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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

import Servant
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, CorsResourcePolicy(..))

import Data.Aeson (FromJSON, ToJSON)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Data.Text (Text, unpack, pack)
import Control.Monad.Reader (ReaderT (runReaderT), MonadReader (ask))
import Database.SQLite.Simple
import Control.Monad.Trans (liftIO)
import Data.List (unfoldr)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Function ((&))
import Common.Function (maybeIf)

import DB (lookupUrl, insertUrl, incrementVisitCount, PrimaryKey (UrlId), UrlT (Url))

makeShortURL :: IO Text
makeShortURL = do
  num <- (+ 10000000000) . round <$> getPOSIXTime
  return $ unfoldr (\b -> maybeIf (b > 0) (let (d, m) = divMod b 62 in (base62!!m, d))) num
         & reverse
         & pack
  where base62 = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

runSqlite :: (Connection -> IO a) -> ReaderT Connection Handler a
runSqlite f = ask >>= (liftIO . f)

data ShortenReq where
  ShortenReq :: {longUrl :: Text} -> ShortenReq
  deriving (Show, Eq, Generic)
instance FromJSON ShortenReq where

data ShortenRes where
  ShortenRes :: {shortUrl :: Text} -> ShortenRes
  deriving (Show, Eq, Generic)
instance ToJSON ShortenRes where

data RedirectRes where
  RedirectRes :: {longUrl :: Text} -> RedirectRes
  deriving (Show, Eq, Generic)
instance ToJSON RedirectRes where

data StatsRes = StatsRes
  { shortUrl :: String
  , longUrl  :: String
  , visitCount :: Int
  } deriving (Show, Eq, Generic)
instance ToJSON StatsRes where

type AppEx54 = ReaderT Connection Handler

type API =  "ex54" :> ReqBody '[JSON] ShortenReq         :> Post '[JSON] ShortenRes
       :<|> "ex54" :> Capture "shortUrl" Text            :> Get  '[JSON] NoContent
       :<|> "ex54" :> Capture "shortUrl" Text :> "stats" :> Get  '[JSON] StatsRes

shortenUrl :: ShortenReq -> AppEx54 ShortenRes
shortenUrl (ShortenReq longUrl') = do
  shortUrl' <- liftIO makeShortURL
  _         <- runSqlite $ insertUrl shortUrl' longUrl'
  return $ ShortenRes { shortUrl = shortUrl' }

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
