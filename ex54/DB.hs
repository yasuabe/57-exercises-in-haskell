{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}

-- module DB(Url(..)) where
module DB (lookupUrl, insertUrl, incrementVisitCount, PrimaryKey (UrlId), Url, UrlT(..)) where

import Database.Beam
import Database.Beam.Sqlite
import Data.Text (Text)
import Data.Int(Int32)
import Database.SQLite.Simple

data UrlT f
  = Url
  { _urlShortUrl   :: Columnar f Text
  , _urlLongUrl    :: Columnar f Text
  , _urlVisitCount :: Columnar f Int32}
  deriving Generic

type Url = UrlT Identity
type UrlId = PrimaryKey UrlT Identity

instance Beamable UrlT

instance Table UrlT where
   data PrimaryKey UrlT f = UrlId (Columnar f Text) deriving (Generic, Beamable)
   primaryKey = UrlId . _urlShortUrl

data UrlShortnerDb f where
  UrlShortnerDb :: {_url :: f (TableEntity UrlT)} -> UrlShortnerDb f
  deriving (Generic, Database be)

urlShortnerDb :: DatabaseSettings be UrlShortnerDb
urlShortnerDb = defaultDbSettings

deriving instance Show Url
deriving instance Eq Url

lookupUrl :: UrlId -> Connection -> IO (Maybe Url)
lookupUrl urlId conn
  = runBeamSqliteDebug putStrLn conn
  $ runSelectReturningOne
  $ lookup_ (_url urlShortnerDb) urlId

insertUrl :: Text -> Text -> Connection -> IO ()
insertUrl short long conn
  = runBeamSqliteDebug putStrLn conn
  $ runInsert
  $ insert (_url urlShortnerDb)
  $ insertValues [ Url short long 0 ]

incrementVisitCount :: Text -> Connection -> IO (Maybe Text)
incrementVisitCount shortUrl conn
  = runBeamSqliteDebug putStrLn conn
  $ do
      runSelectReturningOne (lookup_ (_url urlShortnerDb) (UrlId shortUrl)) >>= \case
        Just (Url _ longUrl count) -> do
          runUpdate $ update (_url urlShortnerDb)
                             (\url -> _urlVisitCount url <-. val_ count + 1)
                             (\url -> _urlShortUrl url ==. val_ shortUrl)
          return $ Just longUrl
        Nothing -> return Nothing
